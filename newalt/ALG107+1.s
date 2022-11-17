fof(f4401, plain, $false, inference(avatar_sat_refutation, [], [f465, f482, f584, f635, f669, f676, f680, f688, f690, f693, f695, f697, f735, f769, f820, f854, f939, f974, f979, f980, f981, f984, f997, f999, f1000, f1001, f1004, f1005, f1019, f1038, f1048, f1062, f1067, f1072, f1077, f1097, f1111, f1130, f1140, f1154, f1159, f1164, f1189, f1191, f1193, f1246, f1260, f1291, f1521, f1908, f1911, f1913, f1962, f2046, f2055, f2069, f2096, f2162, f2168, f2237, f2254, f2259, f2272, f2323, f2333, f2334, f2335, f2336, f2337, f2340, f2351, f2399, f2405, f2415, f2468, f2511, f2521, f2525, f2527, f2574, f2586, f2603, f2604, f2620, f2624, f2637, f2661, f2663, f2667, f2672, f2680, f2696, f2709, f2712, f2730, f2805, f2808, f2824, f2837, f2895, f2902, f2903, f2904, f2928, f2953, f2963, f2975, f2981, f3037, f3042, f3050, f3053, f3054, f3098, f3121, f3131, f3151, f3195, f3221, f3257, f3266, f3271, f3290, f3303, f3333, f3343, f3360, f3392, f3408, f3440, f3445, f3485, f3549, f3551, f3561, f3584, f3615, f3659, f3689, f3720, f3750, f3769, f3781, f3814, f3840, f3871, f3894, f3920, f3946, f3983, f4017, f4031, f4077, f4139, f4157, f4253, f4274, f4297, f4322, f4323])).
fof(f4323, plain, (spl18_8 | ~ spl18_51 | ~ spl18_140), inference(avatar_split_clause, [], [f4317, f1059, f611, f428])).
fof(f428, plain, (spl18_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_8])])).
fof(f611, plain, (spl18_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_51])])).
fof(f1059, plain, (spl18_140 <=> (e13 = op1(e13, op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_140])])).
fof(f4317, plain, ((e13 = op1(e13, e12)) | (~ spl18_51 | ~ spl18_140)), inference(backward_demodulation, [], [f1061, f613])).
fof(f613, plain, ((e12 = op1(e10, e13)) | ~ spl18_51), inference(avatar_component_clause, [], [f611])).
fof(f1061, plain, ((e13 = op1(e13, op1(e10, e13))) | ~ spl18_140), inference(avatar_component_clause, [], [f1059])).
fof(f4322, plain, (~ spl18_19 | ~ spl18_51), inference(avatar_split_clause, [], [f4314, f611, f475])).
fof(f475, plain, (spl18_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_19])])).
fof(f4314, plain, (~ (e12 = op1(e12, e13)) | ~ spl18_51), inference(backward_demodulation, [], [f175, f613])).
fof(f175, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax5)).
fof(f4297, plain, (spl18_34 | ~ spl18_60 | ~ spl18_142), inference(avatar_split_clause, [], [f4290, f1069, f649, f539])).
fof(f539, plain, (spl18_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_34])])).
fof(f649, plain, (spl18_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_60])])).
fof(f1069, plain, (spl18_142 <=> (e11 = op1(e11, op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_142])])).
fof(f4290, plain, ((e11 = op1(e11, e13)) | (~ spl18_60 | ~ spl18_142)), inference(backward_demodulation, [], [f1071, f651])).
fof(f651, plain, ((e13 = op1(e10, e11)) | ~ spl18_60), inference(avatar_component_clause, [], [f649])).
fof(f1071, plain, ((e11 = op1(e11, op1(e10, e11))) | ~ spl18_142), inference(avatar_component_clause, [], [f1069])).
fof(f4274, plain, (~ spl18_72 | ~ spl18_120), inference(avatar_split_clause, [], [f4270, f936, f732])).
fof(f732, plain, (spl18_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_72])])).
fof(f936, plain, (spl18_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_120])])).
fof(f4270, plain, (~ (e23 = op2(e23, e22)) | ~ spl18_120), inference(backward_demodulation, [], [f218, f938])).
fof(f938, plain, ((e23 = op2(e20, e22)) | ~ spl18_120), inference(avatar_component_clause, [], [f936])).
fof(f218, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax6)).
fof(f4253, plain, (~ spl18_79 | spl18_117 | ~ spl18_166), inference(avatar_contradiction_clause, [], [f4252])).
fof(f4252, plain, ($false | (~ spl18_79 | spl18_117 | ~ spl18_166)), inference(subsumption_resolution, [], [f4251, f925])).
fof(f925, plain, (~ (e20 = op2(e20, e22)) | spl18_117), inference(avatar_component_clause, [], [f924])).
fof(f924, plain, (spl18_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_117])])).
fof(f4251, plain, ((e20 = op2(e20, e22)) | (~ spl18_79 | ~ spl18_166)), inference(forward_demodulation, [], [f1188, f764])).
fof(f764, plain, ((e22 = op2(e23, e20)) | ~ spl18_79), inference(avatar_component_clause, [], [f762])).
fof(f762, plain, (spl18_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_79])])).
fof(f1188, plain, ((e20 = op2(e20, op2(e23, e20))) | ~ spl18_166), inference(avatar_component_clause, [], [f1186])).
fof(f1186, plain, (spl18_166 <=> (e20 = op2(e20, op2(e23, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_166])])).
fof(f4157, plain, (~ spl18_33 | ~ spl18_72 | ~ spl18_182 | ~ spl18_192 | ~ spl18_245 | ~ spl18_258), inference(avatar_contradiction_clause, [], [f4156])).
fof(f4156, plain, ($false | (~ spl18_33 | ~ spl18_72 | ~ spl18_182 | ~ spl18_192 | ~ spl18_245 | ~ spl18_258)), inference(subsumption_resolution, [], [f4155, f260])).
fof(f260, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax8)).
fof(f4155, plain, ((e20 = e23) | (~ spl18_33 | ~ spl18_72 | ~ spl18_182 | ~ spl18_192 | ~ spl18_245 | ~ spl18_258)), inference(forward_demodulation, [], [f4147, f1367])).
fof(f1367, plain, ((e20 = h2(e10)) | ~ spl18_192), inference(avatar_component_clause, [], [f1366])).
fof(f1366, plain, (spl18_192 <=> (e20 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_192])])).
fof(f4147, plain, ((e23 = h2(e10)) | (~ spl18_33 | ~ spl18_72 | ~ spl18_182 | ~ spl18_245 | ~ spl18_258)), inference(backward_demodulation, [], [f4024, f537])).
fof(f537, plain, ((e10 = op1(e11, e13)) | ~ spl18_33), inference(avatar_component_clause, [], [f535])).
fof(f535, plain, (spl18_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_33])])).
fof(f4024, plain, ((e23 = h2(op1(e11, e13))) | (~ spl18_72 | ~ spl18_182 | ~ spl18_245 | ~ spl18_258)), inference(forward_demodulation, [], [f3798, f734])).
fof(f734, plain, ((e23 = op2(e23, e22)) | ~ spl18_72), inference(avatar_component_clause, [], [f732])).
fof(f3798, plain, ((op2(e23, e22) = h2(op1(e11, e13))) | (~ spl18_182 | ~ spl18_245 | ~ spl18_258)), inference(forward_demodulation, [], [f3797, f1777])).
fof(f1777, plain, ((e23 = h2(e11)) | ~ spl18_258), inference(avatar_component_clause, [], [f1776])).
fof(f1776, plain, (spl18_258 <=> (e23 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_258])])).
fof(f3797, plain, ((h2(op1(e11, e13)) = op2(h2(e11), e22)) | (~ spl18_182 | ~ spl18_245)), inference(forward_demodulation, [], [f1713, f1315])).
fof(f1315, plain, ((e22 = h2(e13)) | ~ spl18_182), inference(avatar_component_clause, [], [f1314])).
fof(f1314, plain, (spl18_182 <=> (e22 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_182])])).
fof(f1713, plain, ((h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ spl18_245), inference(avatar_component_clause, [], [f1712])).
fof(f1712, plain, (spl18_245 <=> (h2(op1(e11, e13)) = op2(h2(e11), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_245])])).
fof(f4139, plain, (~ spl18_38 | ~ spl18_39), inference(avatar_contradiction_clause, [], [f4138])).
fof(f4138, plain, ($false | (~ spl18_38 | ~ spl18_39)), inference(subsumption_resolution, [], [f4137, f255])).
fof(f255, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax7)).
fof(f4137, plain, ((e11 = e12) | (~ spl18_38 | ~ spl18_39)), inference(backward_demodulation, [], [f562, f558])).
fof(f558, plain, ((e11 = op1(e11, e12)) | ~ spl18_38), inference(avatar_component_clause, [], [f556])).
fof(f556, plain, (spl18_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_38])])).
fof(f562, plain, ((e12 = op1(e11, e12)) | ~ spl18_39), inference(avatar_component_clause, [], [f560])).
fof(f560, plain, (spl18_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_39])])).
fof(f4077, plain, (spl18_38 | ~ spl18_59 | ~ spl18_142), inference(avatar_split_clause, [], [f4069, f1069, f645, f556])).
fof(f645, plain, (spl18_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_59])])).
fof(f4069, plain, ((e11 = op1(e11, e12)) | (~ spl18_59 | ~ spl18_142)), inference(backward_demodulation, [], [f1071, f647])).
fof(f647, plain, ((e12 = op1(e10, e11)) | ~ spl18_59), inference(avatar_component_clause, [], [f645])).
fof(f4031, plain, (~ spl18_43 | ~ spl18_39), inference(avatar_split_clause, [], [f3507, f560, f577])).
fof(f577, plain, (spl18_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_43])])).
fof(f3507, plain, (~ (e12 = op1(e11, e11)) | ~ spl18_39), inference(forward_demodulation, [], [f189, f562])).
fof(f189, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4017, plain, (~ spl18_26 | ~ spl18_30), inference(avatar_split_clause, [], [f3341, f522, f505])).
fof(f505, plain, (spl18_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_26])])).
fof(f522, plain, (spl18_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_30])])).
fof(f3341, plain, (~ (e11 = op1(e12, e11)) | ~ spl18_30), inference(forward_demodulation, [], [f192, f524])).
fof(f524, plain, ((e11 = op1(e12, e10)) | ~ spl18_30), inference(avatar_component_clause, [], [f522])).
fof(f192, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3983, plain, (~ spl18_48 | spl18_220), inference(avatar_contradiction_clause, [], [f3982])).
fof(f3982, plain, ($false | (~ spl18_48 | spl18_220)), inference(trivial_inequality_removal, [], [f3981])).
fof(f3981, plain, (~ (h4(e13) = h4(e13)) | (~ spl18_48 | spl18_220)), inference(forward_demodulation, [], [f1504, f600])).
fof(f600, plain, ((e13 = op1(e11, e10)) | ~ spl18_48), inference(avatar_component_clause, [], [f598])).
fof(f598, plain, (spl18_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_48])])).
fof(f1504, plain, (~ (h4(e13) = h4(op1(e11, e10))) | spl18_220), inference(avatar_component_clause, [], [f1502])).
fof(f1502, plain, (spl18_220 <=> (h4(e13) = h4(op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_220])])).
fof(f3946, plain, (~ spl18_54 | ~ spl18_115 | ~ spl18_169 | ~ spl18_178 | spl18_221), inference(avatar_contradiction_clause, [], [f3945])).
fof(f3945, plain, ($false | (~ spl18_54 | ~ spl18_115 | ~ spl18_169 | ~ spl18_178 | spl18_221)), inference(subsumption_resolution, [], [f3944, f917])).
fof(f917, plain, ((e22 = op2(e20, e23)) | ~ spl18_115), inference(avatar_component_clause, [], [f915])).
fof(f915, plain, (spl18_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_115])])).
fof(f3944, plain, (~ (e22 = op2(e20, e23)) | (~ spl18_54 | ~ spl18_169 | ~ spl18_178 | spl18_221)), inference(forward_demodulation, [], [f3943, f1244])).
fof(f1244, plain, ((e22 = h4(e11)) | ~ spl18_169), inference(avatar_component_clause, [], [f1243])).
fof(f1243, plain, (spl18_169 <=> (e22 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_169])])).
fof(f3943, plain, (~ (op2(e20, e23) = h4(e11)) | (~ spl18_54 | ~ spl18_178 | spl18_221)), inference(forward_demodulation, [], [f3942, f626])).
fof(f626, plain, ((e11 = op1(e10, e12)) | ~ spl18_54), inference(avatar_component_clause, [], [f624])).
fof(f624, plain, (spl18_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_54])])).
fof(f3942, plain, (~ (op2(e20, e23) = h4(op1(e10, e12))) | (~ spl18_178 | spl18_221)), inference(forward_demodulation, [], [f1508, f1289])).
fof(f1289, plain, ((e20 = h4(e10)) | ~ spl18_178), inference(avatar_component_clause, [], [f1288])).
fof(f1288, plain, (spl18_178 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_178])])).
fof(f1508, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e23)) | spl18_221), inference(avatar_component_clause, [], [f1506])).
fof(f1506, plain, (spl18_221 <=> (h4(op1(e10, e12)) = op2(h4(e10), e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_221])])).
fof(f3920, plain, (~ spl18_39 | ~ spl18_84 | ~ spl18_169 | spl18_219), inference(avatar_contradiction_clause, [], [f3919])).
fof(f3919, plain, ($false | (~ spl18_39 | ~ spl18_84 | ~ spl18_169 | spl18_219)), inference(subsumption_resolution, [], [f3918, f785])).
fof(f785, plain, ((e23 = op2(e22, e23)) | ~ spl18_84), inference(avatar_component_clause, [], [f783])).
fof(f783, plain, (spl18_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_84])])).
fof(f3918, plain, (~ (e23 = op2(e22, e23)) | (~ spl18_39 | ~ spl18_169 | spl18_219)), inference(forward_demodulation, [], [f3917, f330])).
fof(f330, plain, (e23 = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((h4(e13) = op2(op2(e23, op2(e23, e23)), op2(e23, e23))) & (op2(e23, op2(e23, e23)) = h4(e11)) & (op2(e23, e23) = h4(e10)) & (e23 = h4(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax17)).
fof(f3917, plain, (~ (op2(e22, e23) = h4(e12)) | (~ spl18_39 | ~ spl18_169 | spl18_219)), inference(forward_demodulation, [], [f3916, f562])).
fof(f3916, plain, (~ (op2(e22, e23) = h4(op1(e11, e12))) | (~ spl18_169 | spl18_219)), inference(forward_demodulation, [], [f1500, f1244])).
fof(f1500, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e23)) | spl18_219), inference(avatar_component_clause, [], [f1498])).
fof(f1498, plain, (spl18_219 <=> (h4(op1(e11, e12)) = op2(h4(e11), e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_219])])).
fof(f3894, plain, (~ spl18_30 | spl18_218), inference(avatar_contradiction_clause, [], [f3893])).
fof(f3893, plain, ($false | (~ spl18_30 | spl18_218)), inference(trivial_inequality_removal, [], [f3892])).
fof(f3892, plain, (~ (h4(e11) = h4(e11)) | (~ spl18_30 | spl18_218)), inference(forward_demodulation, [], [f1496, f524])).
fof(f1496, plain, (~ (h4(e11) = h4(op1(e12, e10))) | spl18_218), inference(avatar_component_clause, [], [f1494])).
fof(f1494, plain, (spl18_218 <=> (h4(e11) = h4(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_218])])).
fof(f3871, plain, (~ spl18_21 | spl18_216), inference(avatar_contradiction_clause, [], [f3870])).
fof(f3870, plain, ($false | (~ spl18_21 | spl18_216)), inference(trivial_inequality_removal, [], [f3869])).
fof(f3869, plain, (~ (h4(e10) = h4(e10)) | (~ spl18_21 | spl18_216)), inference(forward_demodulation, [], [f1488, f486])).
fof(f486, plain, ((e10 = op1(e12, e12)) | ~ spl18_21), inference(avatar_component_clause, [], [f484])).
fof(f484, plain, (spl18_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_21])])).
fof(f1488, plain, (~ (h4(e10) = h4(op1(e12, e12))) | spl18_216), inference(avatar_component_clause, [], [f1486])).
fof(f1486, plain, (spl18_216 <=> (h4(e10) = h4(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_216])])).
fof(f3840, plain, (~ spl18_27 | ~ spl18_72 | ~ spl18_169 | spl18_217), inference(avatar_contradiction_clause, [], [f3839])).
fof(f3839, plain, ($false | (~ spl18_27 | ~ spl18_72 | ~ spl18_169 | spl18_217)), inference(subsumption_resolution, [], [f3838, f734])).
fof(f3838, plain, (~ (e23 = op2(e23, e22)) | (~ spl18_27 | ~ spl18_169 | spl18_217)), inference(forward_demodulation, [], [f3837, f330])).
fof(f3837, plain, (~ (op2(e23, e22) = h4(e12)) | (~ spl18_27 | ~ spl18_169 | spl18_217)), inference(forward_demodulation, [], [f3836, f511])).
fof(f511, plain, ((e12 = op1(e12, e11)) | ~ spl18_27), inference(avatar_component_clause, [], [f509])).
fof(f509, plain, (spl18_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_27])])).
fof(f3836, plain, (~ (op2(e23, e22) = h4(op1(e12, e11))) | (~ spl18_169 | spl18_217)), inference(forward_demodulation, [], [f1492, f1244])).
fof(f1492, plain, (~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | spl18_217), inference(avatar_component_clause, [], [f1490])).
fof(f1490, plain, (spl18_217 <=> (h4(op1(e12, e11)) = op2(e23, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_217])])).
fof(f3814, plain, (~ spl18_8 | ~ spl18_98 | ~ spl18_172 | spl18_214), inference(avatar_contradiction_clause, [], [f3813])).
fof(f3813, plain, ($false | (~ spl18_8 | ~ spl18_98 | ~ spl18_172 | spl18_214)), inference(subsumption_resolution, [], [f3812, f845])).
fof(f845, plain, ((e21 = op2(e21, e23)) | ~ spl18_98), inference(avatar_component_clause, [], [f843])).
fof(f843, plain, (spl18_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_98])])).
fof(f3812, plain, (~ (e21 = op2(e21, e23)) | (~ spl18_8 | ~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f3811, f1258])).
fof(f1258, plain, ((e21 = h4(e13)) | ~ spl18_172), inference(avatar_component_clause, [], [f1257])).
fof(f1257, plain, (spl18_172 <=> (e21 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_172])])).
fof(f3811, plain, (~ (op2(e21, e23) = h4(e13)) | (~ spl18_8 | ~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f3810, f430])).
fof(f430, plain, ((e13 = op1(e13, e12)) | ~ spl18_8), inference(avatar_component_clause, [], [f428])).
fof(f3810, plain, (~ (op2(e21, e23) = h4(op1(e13, e12))) | (~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f1480, f1258])).
fof(f1480, plain, (~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | spl18_214), inference(avatar_component_clause, [], [f1478])).
fof(f1478, plain, (spl18_214 <=> (h4(op1(e13, e12)) = op2(h4(e13), e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_214])])).
fof(f3781, plain, (~ spl18_20 | ~ spl18_74 | ~ spl18_172 | spl18_215), inference(avatar_contradiction_clause, [], [f3780])).
fof(f3780, plain, ($false | (~ spl18_20 | ~ spl18_74 | ~ spl18_172 | spl18_215)), inference(subsumption_resolution, [], [f3779, f743])).
fof(f743, plain, ((e21 = op2(e23, e21)) | ~ spl18_74), inference(avatar_component_clause, [], [f741])).
fof(f741, plain, (spl18_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_74])])).
fof(f3779, plain, (~ (e21 = op2(e23, e21)) | (~ spl18_20 | ~ spl18_172 | spl18_215)), inference(forward_demodulation, [], [f3778, f1258])).
fof(f3778, plain, (~ (op2(e23, e21) = h4(e13)) | (~ spl18_20 | ~ spl18_172 | spl18_215)), inference(forward_demodulation, [], [f3777, f481])).
fof(f481, plain, ((e13 = op1(e12, e13)) | ~ spl18_20), inference(avatar_component_clause, [], [f479])).
fof(f479, plain, (spl18_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_20])])).
fof(f3777, plain, (~ (op2(e23, e21) = h4(op1(e12, e13))) | (~ spl18_172 | spl18_215)), inference(forward_demodulation, [], [f1484, f1258])).
fof(f1484, plain, (~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | spl18_215), inference(avatar_component_clause, [], [f1482])).
fof(f1482, plain, (spl18_215 <=> (h4(op1(e12, e13)) = op2(e23, h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_215])])).
fof(f3769, plain, (~ spl18_34 | ~ spl18_72 | ~ spl18_182 | spl18_245 | ~ spl18_258), inference(avatar_contradiction_clause, [], [f3768])).
fof(f3768, plain, ($false | (~ spl18_34 | ~ spl18_72 | ~ spl18_182 | spl18_245 | ~ spl18_258)), inference(subsumption_resolution, [], [f3767, f734])).
fof(f3767, plain, (~ (e23 = op2(e23, e22)) | (~ spl18_34 | ~ spl18_182 | spl18_245 | ~ spl18_258)), inference(forward_demodulation, [], [f3766, f1777])).
fof(f3766, plain, (~ (op2(e23, e22) = h2(e11)) | (~ spl18_34 | ~ spl18_182 | spl18_245 | ~ spl18_258)), inference(forward_demodulation, [], [f3765, f541])).
fof(f541, plain, ((e11 = op1(e11, e13)) | ~ spl18_34), inference(avatar_component_clause, [], [f539])).
fof(f3765, plain, (~ (op2(e23, e22) = h2(op1(e11, e13))) | (~ spl18_182 | spl18_245 | ~ spl18_258)), inference(forward_demodulation, [], [f3764, f1777])).
fof(f3764, plain, (~ (h2(op1(e11, e13)) = op2(h2(e11), e22)) | (~ spl18_182 | spl18_245)), inference(forward_demodulation, [], [f1714, f1315])).
fof(f1714, plain, (~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | spl18_245), inference(avatar_component_clause, [], [f1712])).
fof(f3750, plain, (~ spl18_1 | ~ spl18_105 | ~ spl18_172 | ~ spl18_178 | spl18_212), inference(avatar_contradiction_clause, [], [f3749])).
fof(f3749, plain, ($false | (~ spl18_1 | ~ spl18_105 | ~ spl18_172 | ~ spl18_178 | spl18_212)), inference(subsumption_resolution, [], [f3748, f875])).
fof(f875, plain, ((e20 = op2(e21, e21)) | ~ spl18_105), inference(avatar_component_clause, [], [f873])).
fof(f873, plain, (spl18_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_105])])).
fof(f3748, plain, (~ (e20 = op2(e21, e21)) | (~ spl18_1 | ~ spl18_172 | ~ spl18_178 | spl18_212)), inference(forward_demodulation, [], [f3747, f1289])).
fof(f3747, plain, (~ (op2(e21, e21) = h4(e10)) | (~ spl18_1 | ~ spl18_172 | spl18_212)), inference(forward_demodulation, [], [f3746, f401])).
fof(f401, plain, ((e10 = op1(e13, e13)) | ~ spl18_1), inference(avatar_component_clause, [], [f399])).
fof(f399, plain, (spl18_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_1])])).
fof(f3746, plain, (~ (op2(e21, e21) = h4(op1(e13, e13))) | (~ spl18_172 | spl18_212)), inference(forward_demodulation, [], [f1472, f1258])).
fof(f1472, plain, (~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | spl18_212), inference(avatar_component_clause, [], [f1470])).
fof(f1470, plain, (spl18_212 <=> (h4(op1(e13, e13)) = op2(h4(e13), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_212])])).
fof(f3720, plain, (~ spl18_10 | ~ spl18_103 | ~ spl18_169 | ~ spl18_172 | spl18_211), inference(avatar_contradiction_clause, [], [f3719])).
fof(f3719, plain, ($false | (~ spl18_10 | ~ spl18_103 | ~ spl18_169 | ~ spl18_172 | spl18_211)), inference(subsumption_resolution, [], [f3718, f866])).
fof(f866, plain, ((e22 = op2(e21, e22)) | ~ spl18_103), inference(avatar_component_clause, [], [f864])).
fof(f864, plain, (spl18_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_103])])).
fof(f3718, plain, (~ (e22 = op2(e21, e22)) | (~ spl18_10 | ~ spl18_169 | ~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f3717, f1244])).
fof(f3717, plain, (~ (op2(e21, e22) = h4(e11)) | (~ spl18_10 | ~ spl18_169 | ~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f3716, f439])).
fof(f439, plain, ((e11 = op1(e13, e11)) | ~ spl18_10), inference(avatar_component_clause, [], [f437])).
fof(f437, plain, (spl18_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_10])])).
fof(f3716, plain, (~ (op2(e21, e22) = h4(op1(e13, e11))) | (~ spl18_169 | ~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f3715, f1258])).
fof(f3715, plain, (~ (h4(op1(e13, e11)) = op2(h4(e13), e22)) | (~ spl18_169 | spl18_211)), inference(forward_demodulation, [], [f1468, f1244])).
fof(f1468, plain, (~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | spl18_211), inference(avatar_component_clause, [], [f1466])).
fof(f1466, plain, (spl18_211 <=> (h4(op1(e13, e11)) = op2(h4(e13), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_211])])).
fof(f3689, plain, (~ spl18_15 | ~ spl18_112 | ~ spl18_172 | ~ spl18_178 | spl18_210), inference(avatar_contradiction_clause, [], [f3688])).
fof(f3688, plain, ($false | (~ spl18_15 | ~ spl18_112 | ~ spl18_172 | ~ spl18_178 | spl18_210)), inference(subsumption_resolution, [], [f3687, f904])).
fof(f904, plain, ((e23 = op2(e21, e20)) | ~ spl18_112), inference(avatar_component_clause, [], [f902])).
fof(f902, plain, (spl18_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_112])])).
fof(f3687, plain, (~ (e23 = op2(e21, e20)) | (~ spl18_15 | ~ spl18_172 | ~ spl18_178 | spl18_210)), inference(forward_demodulation, [], [f3686, f330])).
fof(f3686, plain, (~ (op2(e21, e20) = h4(e12)) | (~ spl18_15 | ~ spl18_172 | ~ spl18_178 | spl18_210)), inference(forward_demodulation, [], [f3685, f460])).
fof(f460, plain, ((e12 = op1(e13, e10)) | ~ spl18_15), inference(avatar_component_clause, [], [f458])).
fof(f458, plain, (spl18_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_15])])).
fof(f3685, plain, (~ (op2(e21, e20) = h4(op1(e13, e10))) | (~ spl18_172 | ~ spl18_178 | spl18_210)), inference(forward_demodulation, [], [f3684, f1258])).
fof(f3684, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e20)) | (~ spl18_178 | spl18_210)), inference(forward_demodulation, [], [f1464, f1289])).
fof(f1464, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | spl18_210), inference(avatar_component_clause, [], [f1462])).
fof(f1462, plain, (spl18_210 <=> (h4(op1(e13, e10)) = op2(h4(e13), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_210])])).
fof(f3659, plain, (~ spl18_41 | ~ spl18_85 | ~ spl18_169 | ~ spl18_178 | spl18_208), inference(avatar_contradiction_clause, [], [f3658])).
fof(f3658, plain, ($false | (~ spl18_41 | ~ spl18_85 | ~ spl18_169 | ~ spl18_178 | spl18_208)), inference(subsumption_resolution, [], [f3657, f790])).
fof(f790, plain, ((e20 = op2(e22, e22)) | ~ spl18_85), inference(avatar_component_clause, [], [f788])).
fof(f788, plain, (spl18_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_85])])).
fof(f3657, plain, (~ (e20 = op2(e22, e22)) | (~ spl18_41 | ~ spl18_169 | ~ spl18_178 | spl18_208)), inference(forward_demodulation, [], [f3656, f1289])).
fof(f3656, plain, (~ (op2(e22, e22) = h4(e10)) | (~ spl18_41 | ~ spl18_169 | spl18_208)), inference(forward_demodulation, [], [f3655, f571])).
fof(f571, plain, ((e10 = op1(e11, e11)) | ~ spl18_41), inference(avatar_component_clause, [], [f569])).
fof(f569, plain, (spl18_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_41])])).
fof(f3655, plain, (~ (op2(e22, e22) = h4(op1(e11, e11))) | (~ spl18_169 | spl18_208)), inference(forward_demodulation, [], [f1456, f1244])).
fof(f1456, plain, (~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | spl18_208), inference(avatar_component_clause, [], [f1454])).
fof(f1454, plain, (spl18_208 <=> (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_208])])).
fof(f3615, plain, (~ spl18_34 | ~ spl18_91 | ~ spl18_169 | ~ spl18_172 | spl18_209), inference(avatar_contradiction_clause, [], [f3614])).
fof(f3614, plain, ($false | (~ spl18_34 | ~ spl18_91 | ~ spl18_169 | ~ spl18_172 | spl18_209)), inference(subsumption_resolution, [], [f3613, f815])).
fof(f815, plain, ((e22 = op2(e22, e21)) | ~ spl18_91), inference(avatar_component_clause, [], [f813])).
fof(f813, plain, (spl18_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_91])])).
fof(f3613, plain, (~ (e22 = op2(e22, e21)) | (~ spl18_34 | ~ spl18_169 | ~ spl18_172 | spl18_209)), inference(forward_demodulation, [], [f3612, f1244])).
fof(f3612, plain, (~ (op2(e22, e21) = h4(e11)) | (~ spl18_34 | ~ spl18_169 | ~ spl18_172 | spl18_209)), inference(forward_demodulation, [], [f3611, f541])).
fof(f3611, plain, (~ (op2(e22, e21) = h4(op1(e11, e13))) | (~ spl18_169 | ~ spl18_172 | spl18_209)), inference(forward_demodulation, [], [f3610, f1244])).
fof(f3610, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e21)) | (~ spl18_172 | spl18_209)), inference(forward_demodulation, [], [f1460, f1258])).
fof(f1460, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | spl18_209), inference(avatar_component_clause, [], [f1458])).
fof(f1458, plain, (spl18_209 <=> (h4(op1(e11, e13)) = op2(h4(e11), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_209])])).
fof(f3584, plain, (~ spl18_51 | ~ spl18_124 | ~ spl18_172 | ~ spl18_178 | spl18_207), inference(avatar_contradiction_clause, [], [f3583])).
fof(f3583, plain, ($false | (~ spl18_51 | ~ spl18_124 | ~ spl18_172 | ~ spl18_178 | spl18_207)), inference(subsumption_resolution, [], [f3582, f955])).
fof(f955, plain, ((e23 = op2(e20, e21)) | ~ spl18_124), inference(avatar_component_clause, [], [f953])).
fof(f953, plain, (spl18_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_124])])).
fof(f3582, plain, (~ (e23 = op2(e20, e21)) | (~ spl18_51 | ~ spl18_172 | ~ spl18_178 | spl18_207)), inference(backward_demodulation, [], [f3576, f1258])).
fof(f3576, plain, (~ (e23 = op2(e20, h4(e13))) | (~ spl18_51 | ~ spl18_178 | spl18_207)), inference(forward_demodulation, [], [f3575, f330])).
fof(f3575, plain, (~ (h4(e12) = op2(e20, h4(e13))) | (~ spl18_51 | ~ spl18_178 | spl18_207)), inference(forward_demodulation, [], [f3574, f613])).
fof(f3574, plain, (~ (h4(op1(e10, e13)) = op2(e20, h4(e13))) | (~ spl18_178 | spl18_207)), inference(forward_demodulation, [], [f1452, f1289])).
fof(f1452, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | spl18_207), inference(avatar_component_clause, [], [f1450])).
fof(f1450, plain, (spl18_207 <=> (h4(op1(e10, e13)) = op2(h4(e10), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_207])])).
fof(f3561, plain, (spl18_182 | ~ spl18_79 | ~ spl18_192 | ~ spl18_258), inference(avatar_split_clause, [], [f3560, f1776, f1366, f762, f1314])).
fof(f3560, plain, ((e22 = h2(e13)) | (~ spl18_79 | ~ spl18_192 | ~ spl18_258)), inference(forward_demodulation, [], [f3516, f764])).
fof(f3516, plain, ((op2(e23, e20) = h2(e13)) | (~ spl18_192 | ~ spl18_258)), inference(forward_demodulation, [], [f3515, f1777])).
fof(f3515, plain, ((h2(e13) = op2(h2(e11), e20)) | ~ spl18_192), inference(forward_demodulation, [], [f1211, f1367])).
fof(f1211, plain, (h2(e13) = op2(h2(e11), h2(e10))), inference(backward_demodulation, [], [f1203, f323])).
fof(f323, plain, (op2(e21, e21) = h2(e10)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((h2(e13) = op2(op2(e21, op2(e21, e21)), op2(e21, e21))) & (op2(e21, op2(e21, e21)) = h2(e11)) & (op2(e21, e21) = h2(e10)) & (e21 = h2(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax15)).
fof(f1203, plain, (h2(e13) = op2(h2(e11), op2(e21, e21))), inference(backward_demodulation, [], [f325, f324])).
fof(f324, plain, (op2(e21, op2(e21, e21)) = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f325, plain, (h2(e13) = op2(op2(e21, op2(e21, e21)), op2(e21, e21))), inference(cnf_transformation, [], [f15])).
fof(f3551, plain, (spl18_172 | ~ spl18_94 | ~ spl18_169 | ~ spl18_178), inference(avatar_split_clause, [], [f3550, f1288, f1243, f826, f1257])).
fof(f826, plain, (spl18_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_94])])).
fof(f3550, plain, ((e21 = h4(e13)) | (~ spl18_94 | ~ spl18_169 | ~ spl18_178)), inference(forward_demodulation, [], [f3547, f828])).
fof(f828, plain, ((e21 = op2(e22, e20)) | ~ spl18_94), inference(avatar_component_clause, [], [f826])).
fof(f3547, plain, ((op2(e22, e20) = h4(e13)) | (~ spl18_169 | ~ spl18_178)), inference(backward_demodulation, [], [f3399, f1244])).
fof(f3399, plain, ((h4(e13) = op2(h4(e11), e20)) | ~ spl18_178), inference(backward_demodulation, [], [f1231, f1289])).
fof(f1231, plain, (h4(e13) = op2(h4(e11), h4(e10))), inference(backward_demodulation, [], [f1223, f331])).
fof(f331, plain, (op2(e23, e23) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f1223, plain, (h4(e13) = op2(h4(e11), op2(e23, e23))), inference(backward_demodulation, [], [f333, f332])).
fof(f332, plain, (op2(e23, op2(e23, e23)) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f333, plain, (h4(e13) = op2(op2(e23, op2(e23, e23)), op2(e23, e23))), inference(cnf_transformation, [], [f17])).
fof(f3549, plain, (~ spl18_172 | ~ spl18_60 | ~ spl18_118 | ~ spl18_169 | ~ spl18_178 | spl18_206), inference(avatar_split_clause, [], [f3548, f1446, f1288, f1243, f928, f649, f1257])).
fof(f928, plain, (spl18_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_118])])).
fof(f1446, plain, (spl18_206 <=> (h4(op1(e10, e11)) = op2(h4(e10), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_206])])).
fof(f3548, plain, (~ (e21 = h4(e13)) | (~ spl18_60 | ~ spl18_118 | ~ spl18_169 | ~ spl18_178 | spl18_206)), inference(forward_demodulation, [], [f3546, f930])).
fof(f930, plain, ((e21 = op2(e20, e22)) | ~ spl18_118), inference(avatar_component_clause, [], [f928])).
fof(f3546, plain, (~ (op2(e20, e22) = h4(e13)) | (~ spl18_60 | ~ spl18_169 | ~ spl18_178 | spl18_206)), inference(backward_demodulation, [], [f3542, f1244])).
fof(f3542, plain, (~ (h4(e13) = op2(e20, h4(e11))) | (~ spl18_60 | ~ spl18_178 | spl18_206)), inference(forward_demodulation, [], [f3541, f651])).
fof(f3541, plain, (~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | (~ spl18_178 | spl18_206)), inference(forward_demodulation, [], [f1448, f1289])).
fof(f1448, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | spl18_206), inference(avatar_component_clause, [], [f1446])).
fof(f3485, plain, (~ spl18_11 | ~ spl18_15), inference(avatar_split_clause, [], [f3483, f458, f441])).
fof(f441, plain, (spl18_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_11])])).
fof(f3483, plain, (~ (e12 = op1(e13, e11)) | ~ spl18_15), inference(backward_demodulation, [], [f198, f460])).
fof(f198, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3445, plain, (~ spl18_61 | ~ spl18_125 | ~ spl18_178 | spl18_205), inference(avatar_contradiction_clause, [], [f3444])).
fof(f3444, plain, ($false | (~ spl18_61 | ~ spl18_125 | ~ spl18_178 | spl18_205)), inference(subsumption_resolution, [], [f3439, f1289])).
fof(f3439, plain, (~ (e20 = h4(e10)) | (~ spl18_61 | ~ spl18_125 | ~ spl18_178 | spl18_205)), inference(backward_demodulation, [], [f3409, f656])).
fof(f656, plain, ((e10 = op1(e10, e10)) | ~ spl18_61), inference(avatar_component_clause, [], [f654])).
fof(f654, plain, (spl18_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_61])])).
fof(f3409, plain, (~ (e20 = h4(op1(e10, e10))) | (~ spl18_125 | ~ spl18_178 | spl18_205)), inference(forward_demodulation, [], [f3401, f960])).
fof(f960, plain, ((e20 = op2(e20, e20)) | ~ spl18_125), inference(avatar_component_clause, [], [f958])).
fof(f958, plain, (spl18_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_125])])).
fof(f3401, plain, (~ (op2(e20, e20) = h4(op1(e10, e10))) | (~ spl18_178 | spl18_205)), inference(backward_demodulation, [], [f1444, f1289])).
fof(f1444, plain, (~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10))) | spl18_205), inference(avatar_component_clause, [], [f1442])).
fof(f1442, plain, (spl18_205 <=> (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_205])])).
fof(f3440, plain, (~ spl18_13 | ~ spl18_61), inference(avatar_split_clause, [], [f3431, f654, f450])).
fof(f450, plain, (spl18_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_13])])).
fof(f3431, plain, (~ (e10 = op1(e13, e10)) | ~ spl18_61), inference(backward_demodulation, [], [f158, f656])).
fof(f158, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3408, plain, (spl18_169 | ~ spl18_79 | ~ spl18_178), inference(avatar_split_clause, [], [f3407, f1288, f762, f1243])).
fof(f3407, plain, ((e22 = h4(e11)) | (~ spl18_79 | ~ spl18_178)), inference(forward_demodulation, [], [f3398, f764])).
fof(f3398, plain, ((op2(e23, e20) = h4(e11)) | ~ spl18_178), inference(backward_demodulation, [], [f1230, f1289])).
fof(f1230, plain, (h4(e11) = op2(e23, h4(e10))), inference(backward_demodulation, [], [f332, f331])).
fof(f3392, plain, (~ spl18_73 | ~ spl18_192), inference(avatar_split_clause, [], [f3386, f1366, f737])).
fof(f737, plain, (spl18_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_73])])).
fof(f3386, plain, (~ (e20 = op2(e23, e21)) | ~ spl18_192), inference(backward_demodulation, [], [f1206, f1367])).
fof(f1206, plain, ~ (op2(e23, e21) = h2(e10)), inference(backward_demodulation, [], [f214, f323])).
fof(f214, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3360, plain, (~ spl18_99 | ~ spl18_103), inference(avatar_split_clause, [], [f3359, f864, f847])).
fof(f847, plain, (spl18_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_99])])).
fof(f3359, plain, (~ (e22 = op2(e21, e23)) | ~ spl18_103), inference(forward_demodulation, [], [f239, f866])).
fof(f239, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3343, plain, (~ spl18_34 | ~ spl18_42), inference(avatar_split_clause, [], [f3342, f573, f539])).
fof(f573, plain, (spl18_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_42])])).
fof(f3342, plain, (~ (e11 = op1(e11, e13)) | ~ spl18_42), inference(forward_demodulation, [], [f190, f575])).
fof(f575, plain, ((e11 = op1(e11, e11)) | ~ spl18_42), inference(avatar_component_clause, [], [f573])).
fof(f190, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3333, plain, (~ spl18_13 | spl18_61 | ~ spl18_147), inference(avatar_contradiction_clause, [], [f3332])).
fof(f3332, plain, ($false | (~ spl18_13 | spl18_61 | ~ spl18_147)), inference(subsumption_resolution, [], [f3329, f655])).
fof(f655, plain, (~ (e10 = op1(e10, e10)) | spl18_61), inference(avatar_component_clause, [], [f654])).
fof(f3329, plain, ((e10 = op1(e10, e10)) | (~ spl18_13 | ~ spl18_147)), inference(backward_demodulation, [], [f1096, f452])).
fof(f452, plain, ((e10 = op1(e13, e10)) | ~ spl18_13), inference(avatar_component_clause, [], [f450])).
fof(f1096, plain, ((e10 = op1(e10, op1(e13, e10))) | ~ spl18_147), inference(avatar_component_clause, [], [f1094])).
fof(f1094, plain, (spl18_147 <=> (e10 = op1(e10, op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_147])])).
fof(f3303, plain, (spl18_53 | ~ spl18_63 | ~ spl18_143), inference(avatar_contradiction_clause, [], [f3302])).
fof(f3302, plain, ($false | (spl18_53 | ~ spl18_63 | ~ spl18_143)), inference(subsumption_resolution, [], [f3297, f621])).
fof(f621, plain, (~ (e10 = op1(e10, e12)) | spl18_53), inference(avatar_component_clause, [], [f620])).
fof(f620, plain, (spl18_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_53])])).
fof(f3297, plain, ((e10 = op1(e10, e12)) | (~ spl18_63 | ~ spl18_143)), inference(backward_demodulation, [], [f1076, f664])).
fof(f664, plain, ((op1(e10, e10) = e12) | ~ spl18_63), inference(avatar_component_clause, [], [f662])).
fof(f662, plain, (spl18_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl18_63])])).
fof(f1076, plain, ((e10 = op1(e10, op1(e10, e10))) | ~ spl18_143), inference(avatar_component_clause, [], [f1074])).
fof(f1074, plain, (spl18_143 <=> (e10 = op1(e10, op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_143])])).
fof(f3290, plain, (~ spl18_71 | ~ spl18_79), inference(avatar_split_clause, [], [f3287, f762, f728])).
fof(f728, plain, (spl18_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_71])])).
fof(f3287, plain, (~ (e22 = op2(e23, e22)) | ~ spl18_79), inference(backward_demodulation, [], [f247, f764])).
fof(f247, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f3271, plain, (spl18_72 | ~ spl18_115 | ~ spl18_159), inference(avatar_split_clause, [], [f3269, f1151, f915, f732])).
fof(f1151, plain, (spl18_159 <=> (e23 = op2(e23, op2(e20, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl18_159])])).
fof(f3269, plain, ((e23 = op2(e23, e22)) | (~ spl18_115 | ~ spl18_159)), inference(backward_demodulation, [], [f1153, f917])).
fof(f1153, plain, ((e23 = op2(e23, op2(e20, e23))) | ~ spl18_159), inference(avatar_component_clause, [], [f1151])).
fof(f3266, plain, (spl18_91 | ~ spl18_118 | ~ spl18_160), inference(avatar_split_clause, [], [f3263, f1156, f928, f813])).
fof(f1156, plain, (spl18_160 <=> (e22 = op2(e22, op2(e20, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_160])])).
fof(f3263, plain, ((e22 = op2(e22, e21)) | (~ spl18_118 | ~ spl18_160)), inference(backward_demodulation, [], [f1158, f930])).
fof(f1158, plain, ((e22 = op2(e22, op2(e20, e22))) | ~ spl18_160), inference(avatar_component_clause, [], [f1156])).
fof(f3257, plain, (~ spl18_97 | ~ spl18_124 | ~ spl18_161), inference(avatar_contradiction_clause, [], [f3256])).
fof(f3256, plain, ($false | (~ spl18_97 | ~ spl18_124 | ~ spl18_161)), inference(subsumption_resolution, [], [f3255, f258])).
fof(f258, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f3255, plain, ((e20 = e21) | (~ spl18_97 | ~ spl18_124 | ~ spl18_161)), inference(forward_demodulation, [], [f3251, f841])).
fof(f841, plain, ((e20 = op2(e21, e23)) | ~ spl18_97), inference(avatar_component_clause, [], [f839])).
fof(f839, plain, (spl18_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_97])])).
fof(f3251, plain, ((e21 = op2(e21, e23)) | (~ spl18_124 | ~ spl18_161)), inference(backward_demodulation, [], [f1163, f955])).
fof(f1163, plain, ((e21 = op2(e21, op2(e20, e21))) | ~ spl18_161), inference(avatar_component_clause, [], [f1161])).
fof(f1161, plain, (spl18_161 <=> (e21 = op2(e21, op2(e20, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl18_161])])).
fof(f3221, plain, (spl18_80 | ~ spl18_97 | ~ spl18_154), inference(avatar_split_clause, [], [f3220, f1127, f839, f766])).
fof(f766, plain, (spl18_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_80])])).
fof(f1127, plain, (spl18_154 <=> (e23 = op2(e23, op2(e21, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl18_154])])).
fof(f3220, plain, ((e23 = op2(e23, e20)) | (~ spl18_97 | ~ spl18_154)), inference(forward_demodulation, [], [f1129, f841])).
fof(f1129, plain, ((e23 = op2(e23, op2(e21, e23))) | ~ spl18_154), inference(avatar_component_clause, [], [f1127])).
fof(f3195, plain, (~ spl18_56 | ~ spl18_8), inference(avatar_split_clause, [], [f3194, f428, f632])).
fof(f632, plain, (spl18_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_56])])).
fof(f3194, plain, (~ (e13 = op1(e10, e12)) | ~ spl18_8), inference(forward_demodulation, [], [f170, f430])).
fof(f170, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3151, plain, (~ spl18_125 | ~ spl18_127), inference(avatar_contradiction_clause, [], [f3150])).
fof(f3150, plain, ($false | (~ spl18_125 | ~ spl18_127)), inference(subsumption_resolution, [], [f3148, f259])).
fof(f259, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f3148, plain, ((e20 = e22) | (~ spl18_125 | ~ spl18_127)), inference(backward_demodulation, [], [f968, f960])).
fof(f968, plain, ((op2(e20, e20) = e22) | ~ spl18_127), inference(avatar_component_clause, [], [f966])).
fof(f966, plain, (spl18_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl18_127])])).
fof(f3131, plain, (~ spl18_15 | spl18_53 | ~ spl18_147), inference(avatar_contradiction_clause, [], [f3130])).
fof(f3130, plain, ($false | (~ spl18_15 | spl18_53 | ~ spl18_147)), inference(subsumption_resolution, [], [f3129, f621])).
fof(f3129, plain, ((e10 = op1(e10, e12)) | (~ spl18_15 | ~ spl18_147)), inference(forward_demodulation, [], [f1096, f460])).
fof(f3121, plain, (~ spl18_98 | ~ spl18_174), inference(avatar_split_clause, [], [f3111, f1268, f843])).
fof(f1268, plain, (spl18_174 <=> (e21 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_174])])).
fof(f3111, plain, (~ (e21 = op2(e21, e23)) | ~ spl18_174), inference(backward_demodulation, [], [f1225, f1269])).
fof(f1269, plain, ((e21 = h4(e10)) | ~ spl18_174), inference(avatar_component_clause, [], [f1268])).
fof(f1225, plain, ~ (op2(e21, e23) = h4(e10)), inference(backward_demodulation, [], [f226, f331])).
fof(f226, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3098, plain, (~ spl18_77 | ~ spl18_204), inference(avatar_split_clause, [], [f3090, f1427, f754])).
fof(f754, plain, (spl18_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_77])])).
fof(f1427, plain, (spl18_204 <=> (e20 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_204])])).
fof(f3090, plain, (~ (e20 = op2(e23, e20)) | ~ spl18_204), inference(backward_demodulation, [], [f1197, f1428])).
fof(f1428, plain, ((e20 = h1(e10)) | ~ spl18_204), inference(avatar_component_clause, [], [f1427])).
fof(f1197, plain, ~ (op2(e23, e20) = h1(e10)), inference(backward_demodulation, [], [f206, f319])).
fof(f319, plain, (op2(e20, e20) = h1(e10)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((h1(e13) = op2(op2(e20, op2(e20, e20)), op2(e20, e20))) & (op2(e20, op2(e20, e20)) = h1(e11)) & (op2(e20, e20) = h1(e10)) & (e20 = h1(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax14)).
fof(f206, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3054, plain, (~ spl18_80 | ~ spl18_112), inference(avatar_split_clause, [], [f3052, f902, f766])).
fof(f3052, plain, (~ (e23 = op2(e23, e20)) | ~ spl18_112), inference(backward_demodulation, [], [f208, f904])).
fof(f208, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3053, plain, (~ spl18_100 | ~ spl18_112), inference(avatar_split_clause, [], [f3051, f902, f851])).
fof(f851, plain, (spl18_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_100])])).
fof(f3051, plain, (~ (e23 = op2(e21, e23)) | ~ spl18_112), inference(backward_demodulation, [], [f236, f904])).
fof(f236, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3050, plain, (~ spl18_98 | ~ spl18_102), inference(avatar_split_clause, [], [f3049, f860, f843])).
fof(f860, plain, (spl18_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_102])])).
fof(f3049, plain, (~ (e21 = op2(e21, e23)) | ~ spl18_102), inference(forward_demodulation, [], [f239, f862])).
fof(f862, plain, ((e21 = op2(e21, e22)) | ~ spl18_102), inference(avatar_component_clause, [], [f860])).
fof(f3042, plain, (~ spl18_84 | ~ spl18_92), inference(avatar_split_clause, [], [f3041, f817, f783])).
fof(f817, plain, (spl18_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_92])])).
fof(f3041, plain, (~ (e23 = op2(e22, e23)) | ~ spl18_92), inference(backward_demodulation, [], [f244, f819])).
fof(f819, plain, ((e23 = op2(e22, e21)) | ~ spl18_92), inference(avatar_component_clause, [], [f817])).
fof(f244, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3037, plain, (~ spl18_78 | ~ spl18_94), inference(avatar_split_clause, [], [f3036, f826, f758])).
fof(f758, plain, (spl18_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_78])])).
fof(f3036, plain, (~ (e21 = op2(e23, e20)) | ~ spl18_94), inference(forward_demodulation, [], [f209, f828])).
fof(f209, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2981, plain, (~ spl18_105 | spl18_110 | ~ spl18_156), inference(avatar_contradiction_clause, [], [f2980])).
fof(f2980, plain, ($false | (~ spl18_105 | spl18_110 | ~ spl18_156)), inference(subsumption_resolution, [], [f2977, f895])).
fof(f895, plain, (~ (e21 = op2(e21, e20)) | spl18_110), inference(avatar_component_clause, [], [f894])).
fof(f894, plain, (spl18_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_110])])).
fof(f2977, plain, ((e21 = op2(e21, e20)) | (~ spl18_105 | ~ spl18_156)), inference(backward_demodulation, [], [f1139, f875])).
fof(f1139, plain, ((e21 = op2(e21, op2(e21, e21))) | ~ spl18_156), inference(avatar_component_clause, [], [f1137])).
fof(f1137, plain, (spl18_156 <=> (e21 = op2(e21, op2(e21, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl18_156])])).
fof(f2975, plain, (~ spl18_111 | ~ spl18_112), inference(avatar_contradiction_clause, [], [f2974])).
fof(f2974, plain, ($false | (~ spl18_111 | ~ spl18_112)), inference(subsumption_resolution, [], [f2973, f263])).
fof(f263, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f2973, plain, ((e22 = e23) | (~ spl18_111 | ~ spl18_112)), inference(backward_demodulation, [], [f904, f900])).
fof(f900, plain, ((e22 = op2(e21, e20)) | ~ spl18_111), inference(avatar_component_clause, [], [f898])).
fof(f898, plain, (spl18_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_111])])).
fof(f2963, plain, (spl18_102 | ~ spl18_123 | ~ spl18_161), inference(avatar_split_clause, [], [f2961, f1161, f949, f860])).
fof(f949, plain, (spl18_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_123])])).
fof(f2961, plain, ((e21 = op2(e21, e22)) | (~ spl18_123 | ~ spl18_161)), inference(backward_demodulation, [], [f1163, f951])).
fof(f951, plain, ((e22 = op2(e20, e21)) | ~ spl18_123), inference(avatar_component_clause, [], [f949])).
fof(f2953, plain, (~ spl18_21 | ~ spl18_55 | ~ spl18_141), inference(avatar_contradiction_clause, [], [f2952])).
fof(f2952, plain, ($false | (~ spl18_21 | ~ spl18_55 | ~ spl18_141)), inference(subsumption_resolution, [], [f2951, f253])).
fof(f253, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f2951, plain, ((e10 = e12) | (~ spl18_21 | ~ spl18_55 | ~ spl18_141)), inference(forward_demodulation, [], [f2950, f486])).
fof(f2950, plain, ((e12 = op1(e12, e12)) | (~ spl18_55 | ~ spl18_141)), inference(forward_demodulation, [], [f1066, f630])).
fof(f630, plain, ((e12 = op1(e10, e12)) | ~ spl18_55), inference(avatar_component_clause, [], [f628])).
fof(f628, plain, (spl18_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_55])])).
fof(f1066, plain, ((e12 = op1(e12, op1(e10, e12))) | ~ spl18_141), inference(avatar_component_clause, [], [f1064])).
fof(f1064, plain, (spl18_141 <=> (e12 = op1(e12, op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_141])])).
fof(f2928, plain, (spl18_258 | ~ spl18_112 | ~ spl18_192), inference(avatar_split_clause, [], [f2927, f1366, f902, f1776])).
fof(f2927, plain, ((e23 = h2(e11)) | (~ spl18_112 | ~ spl18_192)), inference(forward_demodulation, [], [f2920, f904])).
fof(f2920, plain, ((op2(e21, e20) = h2(e11)) | ~ spl18_192), inference(backward_demodulation, [], [f1210, f1367])).
fof(f1210, plain, (h2(e11) = op2(e21, h2(e10))), inference(backward_demodulation, [], [f324, f323])).
fof(f2904, plain, (spl18_278 | ~ spl18_128), inference(avatar_split_clause, [], [f2738, f970, f1901])).
fof(f1901, plain, (spl18_278 <=> (e23 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_278])])).
fof(f970, plain, (spl18_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl18_128])])).
fof(f2738, plain, ((e23 = h1(e10)) | ~ spl18_128), inference(backward_demodulation, [], [f319, f972])).
fof(f972, plain, ((op2(e20, e20) = e23) | ~ spl18_128), inference(avatar_component_clause, [], [f970])).
fof(f2903, plain, (~ spl18_278 | ~ spl18_112), inference(avatar_split_clause, [], [f2765, f902, f1901])).
fof(f2765, plain, (~ (e23 = h1(e10)) | ~ spl18_112), inference(backward_demodulation, [], [f1195, f904])).
fof(f1195, plain, ~ (op2(e21, e20) = h1(e10)), inference(backward_demodulation, [], [f204, f319])).
fof(f204, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f2902, plain, (~ spl18_123 | ~ spl18_91), inference(avatar_split_clause, [], [f2901, f813, f949])).
fof(f2901, plain, (~ (e22 = op2(e20, e21)) | ~ spl18_91), inference(forward_demodulation, [], [f211, f815])).
fof(f211, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2895, plain, (~ spl18_77 | ~ spl18_113 | ~ spl18_159), inference(avatar_contradiction_clause, [], [f2894])).
fof(f2894, plain, ($false | (~ spl18_77 | ~ spl18_113 | ~ spl18_159)), inference(subsumption_resolution, [], [f2893, f260])).
fof(f2893, plain, ((e20 = e23) | (~ spl18_77 | ~ spl18_113 | ~ spl18_159)), inference(forward_demodulation, [], [f2892, f756])).
fof(f756, plain, ((e20 = op2(e23, e20)) | ~ spl18_77), inference(avatar_component_clause, [], [f754])).
fof(f2892, plain, ((e23 = op2(e23, e20)) | (~ spl18_113 | ~ spl18_159)), inference(forward_demodulation, [], [f1153, f909])).
fof(f909, plain, ((e20 = op2(e20, e23)) | ~ spl18_113), inference(avatar_component_clause, [], [f907])).
fof(f907, plain, (spl18_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_113])])).
fof(f2837, plain, (~ spl18_21 | ~ spl18_23), inference(avatar_contradiction_clause, [], [f2836])).
fof(f2836, plain, ($false | (~ spl18_21 | ~ spl18_23)), inference(subsumption_resolution, [], [f2835, f253])).
fof(f2835, plain, ((e10 = e12) | (~ spl18_21 | ~ spl18_23)), inference(forward_demodulation, [], [f494, f486])).
fof(f494, plain, ((e12 = op1(e12, e12)) | ~ spl18_23), inference(avatar_component_clause, [], [f492])).
fof(f492, plain, (spl18_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_23])])).
fof(f2824, plain, (~ spl18_41 | spl18_46 | ~ spl18_137), inference(avatar_contradiction_clause, [], [f2823])).
fof(f2823, plain, ($false | (~ spl18_41 | spl18_46 | ~ spl18_137)), inference(subsumption_resolution, [], [f2819, f591])).
fof(f591, plain, (~ (e11 = op1(e11, e10)) | spl18_46), inference(avatar_component_clause, [], [f590])).
fof(f590, plain, (spl18_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_46])])).
fof(f2819, plain, ((e11 = op1(e11, e10)) | (~ spl18_41 | ~ spl18_137)), inference(backward_demodulation, [], [f1047, f571])).
fof(f1047, plain, ((e11 = op1(e11, op1(e11, e11))) | ~ spl18_137), inference(avatar_component_clause, [], [f1045])).
fof(f1045, plain, (spl18_137 <=> (e11 = op1(e11, op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_137])])).
fof(f2808, plain, (~ spl18_68 | spl18_223), inference(avatar_contradiction_clause, [], [f2807])).
fof(f2807, plain, ($false | (~ spl18_68 | spl18_223)), inference(subsumption_resolution, [], [f2806, f1550])).
fof(f1550, plain, (~ (e23 = h4(e10)) | spl18_223), inference(avatar_component_clause, [], [f1548])).
fof(f1548, plain, (spl18_223 <=> (e23 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_223])])).
fof(f2806, plain, ((e23 = h4(e10)) | ~ spl18_68), inference(backward_demodulation, [], [f331, f717])).
fof(f717, plain, ((e23 = op2(e23, e23)) | ~ spl18_68), inference(avatar_component_clause, [], [f715])).
fof(f715, plain, (spl18_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_68])])).
fof(f2805, plain, (~ spl18_70 | ~ spl18_72), inference(avatar_contradiction_clause, [], [f2804])).
fof(f2804, plain, ($false | (~ spl18_70 | ~ spl18_72)), inference(subsumption_resolution, [], [f2803, f262])).
fof(f262, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f2803, plain, ((e21 = e23) | (~ spl18_70 | ~ spl18_72)), inference(forward_demodulation, [], [f734, f726])).
fof(f726, plain, ((e21 = op2(e23, e22)) | ~ spl18_70), inference(avatar_component_clause, [], [f724])).
fof(f724, plain, (spl18_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_70])])).
fof(f2730, plain, (~ spl18_77 | spl18_125 | ~ spl18_166), inference(avatar_contradiction_clause, [], [f2729])).
fof(f2729, plain, ($false | (~ spl18_77 | spl18_125 | ~ spl18_166)), inference(subsumption_resolution, [], [f2728, f959])).
fof(f959, plain, (~ (e20 = op2(e20, e20)) | spl18_125), inference(avatar_component_clause, [], [f958])).
fof(f2728, plain, ((e20 = op2(e20, e20)) | (~ spl18_77 | ~ spl18_166)), inference(forward_demodulation, [], [f1188, f756])).
fof(f2712, plain, (~ spl18_113 | ~ spl18_97), inference(avatar_split_clause, [], [f2711, f839, f907])).
fof(f2711, plain, (~ (e20 = op2(e20, e23)) | ~ spl18_97), inference(forward_demodulation, [], [f222, f841])).
fof(f222, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2709, plain, (~ spl18_110 | ~ spl18_94), inference(avatar_split_clause, [], [f2708, f826, f894])).
fof(f2708, plain, (~ (e21 = op2(e21, e20)) | ~ spl18_94), inference(forward_demodulation, [], [f207, f828])).
fof(f207, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f2696, plain, (~ spl18_90 | ~ spl18_94), inference(avatar_split_clause, [], [f2695, f826, f809])).
fof(f809, plain, (spl18_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_90])])).
fof(f2695, plain, (~ (e21 = op2(e22, e21)) | ~ spl18_94), inference(forward_demodulation, [], [f240, f828])).
fof(f240, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2680, plain, (~ spl18_81 | ~ spl18_180), inference(avatar_split_clause, [], [f2506, f1305, f771])).
fof(f771, plain, (spl18_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_81])])).
fof(f1305, plain, (spl18_180 <=> (e20 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_180])])).
fof(f2506, plain, (~ (e20 = op2(e22, e23)) | ~ spl18_180), inference(forward_demodulation, [], [f1220, f1306])).
fof(f1306, plain, ((e20 = h3(e10)) | ~ spl18_180), inference(avatar_component_clause, [], [f1305])).
fof(f1220, plain, ~ (op2(e22, e23) = h3(e10)), inference(backward_demodulation, [], [f245, f327])).
fof(f327, plain, (op2(e22, e22) = h3(e10)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(op2(e22, op2(e22, e22)), op2(e22, e22)) = h3(e13)) & (op2(e22, op2(e22, e22)) = h3(e11)) & (op2(e22, e22) = h3(e10)) & (e22 = h3(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax16)).
fof(f245, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2672, plain, (~ spl18_102 | ~ spl18_70), inference(avatar_split_clause, [], [f2671, f724, f860])).
fof(f2671, plain, (~ (e21 = op2(e21, e22)) | ~ spl18_70), inference(forward_demodulation, [], [f220, f726])).
fof(f220, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2667, plain, (~ spl18_69 | ~ spl18_180), inference(avatar_split_clause, [], [f2491, f1305, f720])).
fof(f720, plain, (spl18_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_69])])).
fof(f2491, plain, (~ (e20 = op2(e23, e22)) | ~ spl18_180), inference(forward_demodulation, [], [f1217, f1306])).
fof(f1217, plain, ~ (op2(e23, e22) = h3(e10)), inference(backward_demodulation, [], [f221, f327])).
fof(f221, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2663, plain, (~ spl18_64 | ~ spl18_48), inference(avatar_split_clause, [], [f2662, f598, f666])).
fof(f666, plain, (spl18_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl18_64])])).
fof(f2662, plain, (~ (op1(e10, e10) = e13) | ~ spl18_48), inference(forward_demodulation, [], [f156, f600])).
fof(f156, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f2661, plain, (~ spl18_62 | ~ spl18_30), inference(avatar_split_clause, [], [f2660, f522, f658])).
fof(f658, plain, (spl18_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl18_62])])).
fof(f2660, plain, (~ (op1(e10, e10) = e11) | ~ spl18_30), inference(forward_demodulation, [], [f157, f524])).
fof(f157, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2637, plain, (~ spl18_18 | ~ spl18_30), inference(avatar_split_clause, [], [f2636, f522, f471])).
fof(f471, plain, (spl18_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_18])])).
fof(f2636, plain, (~ (e11 = op1(e12, e13)) | ~ spl18_30), inference(forward_demodulation, [], [f194, f524])).
fof(f194, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2624, plain, (~ spl18_7 | ~ spl18_15), inference(avatar_split_clause, [], [f2623, f458, f424])).
fof(f424, plain, (spl18_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_7])])).
fof(f2623, plain, (~ (e12 = op1(e13, e12)) | ~ spl18_15), inference(backward_demodulation, [], [f199, f460])).
fof(f199, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2620, plain, (~ spl18_14 | ~ spl18_30), inference(avatar_split_clause, [], [f2615, f522, f454])).
fof(f454, plain, (spl18_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_14])])).
fof(f2615, plain, (~ (e11 = op1(e13, e10)) | ~ spl18_30), inference(backward_demodulation, [], [f161, f524])).
fof(f161, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2604, plain, (~ spl18_16 | ~ spl18_48), inference(avatar_split_clause, [], [f2599, f598, f462])).
fof(f462, plain, (spl18_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_16])])).
fof(f2599, plain, (~ (e13 = op1(e13, e10)) | ~ spl18_48), inference(backward_demodulation, [], [f160, f600])).
fof(f160, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2603, plain, (~ spl18_44 | ~ spl18_48), inference(avatar_split_clause, [], [f2598, f598, f581])).
fof(f581, plain, (spl18_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_44])])).
fof(f2598, plain, (~ (e13 = op1(e11, e11)) | ~ spl18_48), inference(backward_demodulation, [], [f186, f600])).
fof(f186, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2586, plain, (~ spl18_178 | ~ spl18_77), inference(avatar_split_clause, [], [f2582, f754, f1288])).
fof(f2582, plain, (~ (e20 = h4(e10)) | ~ spl18_77), inference(backward_demodulation, [], [f1227, f756])).
fof(f1227, plain, ~ (op2(e23, e20) = h4(e10)), inference(backward_demodulation, [], [f248, f331])).
fof(f248, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2574, plain, (~ spl18_85 | ~ spl18_87), inference(avatar_contradiction_clause, [], [f2573])).
fof(f2573, plain, ($false | (~ spl18_85 | ~ spl18_87)), inference(subsumption_resolution, [], [f2572, f259])).
fof(f2572, plain, ((e20 = e22) | (~ spl18_85 | ~ spl18_87)), inference(forward_demodulation, [], [f798, f790])).
fof(f798, plain, ((e22 = op2(e22, e22)) | ~ spl18_87), inference(avatar_component_clause, [], [f796])).
fof(f796, plain, (spl18_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_87])])).
fof(f2527, plain, (spl18_68 | ~ spl18_116 | ~ spl18_159), inference(avatar_split_clause, [], [f2526, f1151, f919, f715])).
fof(f919, plain, (spl18_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_116])])).
fof(f2526, plain, ((e23 = op2(e23, e23)) | (~ spl18_116 | ~ spl18_159)), inference(forward_demodulation, [], [f1153, f921])).
fof(f921, plain, ((e23 = op2(e20, e23)) | ~ spl18_116), inference(avatar_component_clause, [], [f919])).
fof(f2525, plain, (~ spl18_85 | ~ spl18_119 | ~ spl18_160), inference(avatar_contradiction_clause, [], [f2524])).
fof(f2524, plain, ($false | (~ spl18_85 | ~ spl18_119 | ~ spl18_160)), inference(subsumption_resolution, [], [f2523, f259])).
fof(f2523, plain, ((e20 = e22) | (~ spl18_85 | ~ spl18_119 | ~ spl18_160)), inference(forward_demodulation, [], [f2522, f790])).
fof(f2522, plain, ((e22 = op2(e22, e22)) | (~ spl18_119 | ~ spl18_160)), inference(forward_demodulation, [], [f1158, f934])).
fof(f934, plain, ((e22 = op2(e20, e22)) | ~ spl18_119), inference(avatar_component_clause, [], [f932])).
fof(f932, plain, (spl18_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_119])])).
fof(f2521, plain, (spl18_110 | ~ spl18_121 | ~ spl18_161), inference(avatar_split_clause, [], [f2520, f1161, f941, f894])).
fof(f941, plain, (spl18_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_121])])).
fof(f2520, plain, ((e21 = op2(e21, e20)) | (~ spl18_121 | ~ spl18_161)), inference(forward_demodulation, [], [f1163, f943])).
fof(f943, plain, ((e20 = op2(e20, e21)) | ~ spl18_121), inference(avatar_component_clause, [], [f941])).
fof(f2511, plain, (spl18_112 | ~ spl18_180), inference(avatar_split_clause, [], [f2331, f1305, f902])).
fof(f2331, plain, ((e23 = op2(e21, e20)) | ~ spl18_180), inference(backward_demodulation, [], [f1222, f1306])).
fof(f1222, plain, (e23 = op2(e21, h3(e10))), inference(backward_demodulation, [], [f1192, f327])).
fof(f1192, plain, (e23 = op2(e21, op2(e22, e22))), inference(backward_demodulation, [], [f317, f316])).
fof(f316, plain, (e21 = op2(e22, op2(e22, e22))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(op2(e22, op2(e22, e22)), op2(e22, e22))) & (e21 = op2(e22, op2(e22, e22))) & (e20 = op2(e22, e22))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax13)).
fof(f317, plain, (e23 = op2(op2(e22, op2(e22, e22)), op2(e22, e22))), inference(cnf_transformation, [], [f13])).
fof(f2468, plain, (spl18_16 | ~ spl18_33 | ~ spl18_135), inference(avatar_split_clause, [], [f2467, f1035, f535, f462])).
fof(f1035, plain, (spl18_135 <=> (e13 = op1(e13, op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_135])])).
fof(f2467, plain, ((e13 = op1(e13, e10)) | (~ spl18_33 | ~ spl18_135)), inference(forward_demodulation, [], [f1037, f537])).
fof(f1037, plain, ((e13 = op1(e13, op1(e11, e13))) | ~ spl18_135), inference(avatar_component_clause, [], [f1035])).
fof(f2415, plain, (~ spl18_30 | ~ spl18_46), inference(avatar_split_clause, [], [f2410, f590, f522])).
fof(f2410, plain, (~ (e11 = op1(e12, e10)) | ~ spl18_46), inference(backward_demodulation, [], [f159, f592])).
fof(f592, plain, ((e11 = op1(e11, e10)) | ~ spl18_46), inference(avatar_component_clause, [], [f590])).
fof(f159, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2405, plain, (~ spl18_54 | ~ spl18_58), inference(avatar_split_clause, [], [f2402, f641, f624])).
fof(f641, plain, (spl18_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_58])])).
fof(f2402, plain, (~ (e11 = op1(e10, e12)) | ~ spl18_58), inference(backward_demodulation, [], [f183, f643])).
fof(f643, plain, ((e11 = op1(e10, e11)) | ~ spl18_58), inference(avatar_component_clause, [], [f641])).
fof(f183, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f2399, plain, (spl18_174 | ~ spl18_66), inference(avatar_split_clause, [], [f2398, f707, f1268])).
fof(f707, plain, (spl18_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_66])])).
fof(f2398, plain, ((e21 = h4(e10)) | ~ spl18_66), inference(backward_demodulation, [], [f331, f709])).
fof(f709, plain, ((e21 = op2(e23, e23)) | ~ spl18_66), inference(avatar_component_clause, [], [f707])).
fof(f2351, plain, (~ spl18_95 | ~ spl18_85 | spl18_150), inference(avatar_split_clause, [], [f2350, f1108, f788, f830])).
fof(f830, plain, (spl18_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_95])])).
fof(f1108, plain, (spl18_150 <=> (e22 = op2(e22, op2(e22, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_150])])).
fof(f2350, plain, (~ (e22 = op2(e22, e20)) | (~ spl18_85 | spl18_150)), inference(forward_demodulation, [], [f1109, f790])).
fof(f1109, plain, (~ (e22 = op2(e22, op2(e22, e22))) | spl18_150), inference(avatar_component_clause, [], [f1108])).
fof(f2340, plain, (~ spl18_109 | ~ spl18_180), inference(avatar_contradiction_clause, [], [f2339])).
fof(f2339, plain, ($false | (~ spl18_109 | ~ spl18_180)), inference(subsumption_resolution, [], [f2338, f260])).
fof(f2338, plain, ((e20 = e23) | (~ spl18_109 | ~ spl18_180)), inference(forward_demodulation, [], [f2331, f892])).
fof(f892, plain, ((e20 = op2(e21, e20)) | ~ spl18_109), inference(avatar_component_clause, [], [f890])).
fof(f890, plain, (spl18_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_109])])).
fof(f2337, plain, (spl18_94 | ~ spl18_180), inference(avatar_split_clause, [], [f2330, f1305, f826])).
fof(f2330, plain, ((e21 = op2(e22, e20)) | ~ spl18_180), inference(backward_demodulation, [], [f1221, f1306])).
fof(f1221, plain, (e21 = op2(e22, h3(e10))), inference(backward_demodulation, [], [f316, f327])).
fof(f2336, plain, (~ spl18_89 | ~ spl18_180), inference(avatar_split_clause, [], [f2329, f1305, f805])).
fof(f805, plain, (spl18_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_89])])).
fof(f2329, plain, (~ (e20 = op2(e22, e21)) | ~ spl18_180), inference(backward_demodulation, [], [f1219, f1306])).
fof(f1219, plain, ~ (op2(e22, e21) = h3(e10)), inference(backward_demodulation, [], [f243, f327])).
fof(f243, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2335, plain, (~ spl18_93 | ~ spl18_180), inference(avatar_split_clause, [], [f2328, f1305, f822])).
fof(f822, plain, (spl18_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_93])])).
fof(f2328, plain, (~ (e20 = op2(e22, e20)) | ~ spl18_180), inference(backward_demodulation, [], [f1218, f1306])).
fof(f1218, plain, ~ (op2(e22, e20) = h3(e10)), inference(backward_demodulation, [], [f241, f327])).
fof(f241, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2334, plain, (~ spl18_101 | ~ spl18_180), inference(avatar_split_clause, [], [f2327, f1305, f856])).
fof(f856, plain, (spl18_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_101])])).
fof(f2327, plain, (~ (e20 = op2(e21, e22)) | ~ spl18_180), inference(backward_demodulation, [], [f1216, f1306])).
fof(f1216, plain, ~ (op2(e21, e22) = h3(e10)), inference(backward_demodulation, [], [f219, f327])).
fof(f219, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2333, plain, (~ spl18_117 | ~ spl18_180), inference(avatar_split_clause, [], [f2326, f1305, f924])).
fof(f2326, plain, (~ (e20 = op2(e20, e22)) | ~ spl18_180), inference(backward_demodulation, [], [f1215, f1306])).
fof(f1215, plain, ~ (op2(e20, e22) = h3(e10)), inference(backward_demodulation, [], [f217, f327])).
fof(f217, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2323, plain, (spl18_180 | ~ spl18_85), inference(avatar_split_clause, [], [f2003, f788, f1305])).
fof(f2003, plain, ((e20 = h3(e10)) | ~ spl18_85), inference(backward_demodulation, [], [f327, f790])).
fof(f2272, plain, (~ spl18_53 | ~ spl18_21), inference(avatar_split_clause, [], [f2271, f484, f620])).
fof(f2271, plain, (~ (e10 = op1(e10, e12)) | ~ spl18_21), inference(forward_demodulation, [], [f169, f486])).
fof(f169, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2259, plain, (spl18_48 | ~ spl18_21), inference(avatar_split_clause, [], [f2157, f484, f598])).
fof(f2157, plain, ((e13 = op1(e11, e10)) | ~ spl18_21), inference(backward_demodulation, [], [f1190, f486])).
fof(f1190, plain, (e13 = op1(e11, op1(e12, e12))), inference(backward_demodulation, [], [f314, f313])).
fof(f313, plain, (e11 = op1(e12, op1(e12, e12))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(op1(e12, op1(e12, e12)), op1(e12, e12))) & (e11 = op1(e12, op1(e12, e12))) & (e10 = op1(e12, e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax12)).
fof(f314, plain, (e13 = op1(op1(e12, op1(e12, e12)), op1(e12, e12))), inference(cnf_transformation, [], [f12])).
fof(f2254, plain, (~ spl18_37 | ~ spl18_21), inference(avatar_split_clause, [], [f2253, f484, f552])).
fof(f552, plain, (spl18_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_37])])).
fof(f2253, plain, (~ (e10 = op1(e11, e12)) | ~ spl18_21), inference(forward_demodulation, [], [f171, f486])).
fof(f171, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2237, plain, (spl18_30 | ~ spl18_21), inference(avatar_split_clause, [], [f2156, f484, f522])).
fof(f2156, plain, ((e11 = op1(e12, e10)) | ~ spl18_21), inference(backward_demodulation, [], [f313, f486])).
fof(f2168, plain, (~ spl18_21 | ~ spl18_45), inference(avatar_contradiction_clause, [], [f2167])).
fof(f2167, plain, ($false | (~ spl18_21 | ~ spl18_45)), inference(subsumption_resolution, [], [f2166, f254])).
fof(f254, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f2166, plain, ((e10 = e13) | (~ spl18_21 | ~ spl18_45)), inference(forward_demodulation, [], [f2157, f588])).
fof(f588, plain, ((e10 = op1(e11, e10)) | ~ spl18_45), inference(avatar_component_clause, [], [f586])).
fof(f586, plain, (spl18_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_45])])).
fof(f2162, plain, (~ spl18_17 | ~ spl18_21), inference(avatar_split_clause, [], [f2155, f484, f467])).
fof(f467, plain, (spl18_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_17])])).
fof(f2155, plain, (~ (e10 = op1(e12, e13)) | ~ spl18_21), inference(backward_demodulation, [], [f197, f486])).
fof(f197, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2096, plain, (~ spl18_51 | ~ spl18_52), inference(avatar_contradiction_clause, [], [f2095])).
fof(f2095, plain, ($false | (~ spl18_51 | ~ spl18_52)), inference(subsumption_resolution, [], [f2094, f257])).
fof(f257, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f2094, plain, ((e12 = e13) | (~ spl18_51 | ~ spl18_52)), inference(backward_demodulation, [], [f617, f613])).
fof(f617, plain, ((e13 = op1(e10, e13)) | ~ spl18_52), inference(avatar_component_clause, [], [f615])).
fof(f615, plain, (spl18_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_52])])).
fof(f2069, plain, (~ spl18_49 | ~ spl18_61), inference(avatar_split_clause, [], [f2061, f654, f603])).
fof(f603, plain, (spl18_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_49])])).
fof(f2061, plain, (~ (e10 = op1(e10, e13)) | ~ spl18_61), inference(backward_demodulation, [], [f182, f656])).
fof(f182, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2055, plain, (spl18_178 | ~ spl18_65), inference(avatar_split_clause, [], [f2054, f703, f1288])).
fof(f703, plain, (spl18_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_65])])).
fof(f2054, plain, ((e20 = h4(e10)) | ~ spl18_65), inference(backward_demodulation, [], [f331, f705])).
fof(f705, plain, ((e20 = op2(e23, e23)) | ~ spl18_65), inference(avatar_component_clause, [], [f703])).
fof(f2046, plain, (~ spl18_223 | ~ spl18_72), inference(avatar_split_clause, [], [f2044, f732, f1548])).
fof(f2044, plain, (~ (e23 = h4(e10)) | ~ spl18_72), inference(backward_demodulation, [], [f1229, f734])).
fof(f1229, plain, ~ (op2(e23, e22) = h4(e10)), inference(backward_demodulation, [], [f251, f331])).
fof(f251, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f1962, plain, (spl18_192 | ~ spl18_105), inference(avatar_split_clause, [], [f1961, f873, f1366])).
fof(f1961, plain, ((e20 = h2(e10)) | ~ spl18_105), inference(backward_demodulation, [], [f323, f875])).
fof(f1913, plain, (spl18_204 | ~ spl18_125), inference(avatar_split_clause, [], [f1912, f958, f1427])).
fof(f1912, plain, ((e20 = h1(e10)) | ~ spl18_125), inference(backward_demodulation, [], [f319, f960])).
fof(f1911, plain, ~ spl18_131, inference(avatar_contradiction_clause, [], [f1910])).
fof(f1910, plain, ($false | ~ spl18_131), inference(subsumption_resolution, [], [f1909, f255])).
fof(f1909, plain, ((e11 = e12) | ~ spl18_131), inference(forward_demodulation, [], [f1018, f313])).
fof(f1018, plain, ((e12 = op1(e12, op1(e12, e12))) | ~ spl18_131), inference(avatar_component_clause, [], [f1016])).
fof(f1016, plain, (spl18_131 <=> (e12 = op1(e12, op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_131])])).
fof(f1908, plain, ~ spl18_150, inference(avatar_contradiction_clause, [], [f1907])).
fof(f1907, plain, ($false | ~ spl18_150), inference(subsumption_resolution, [], [f1906, f261])).
fof(f261, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f1906, plain, ((e21 = e22) | ~ spl18_150), inference(forward_demodulation, [], [f1905, f1221])).
fof(f1905, plain, ((e22 = op2(e22, h3(e10))) | ~ spl18_150), inference(forward_demodulation, [], [f1110, f327])).
fof(f1110, plain, ((e22 = op2(e22, op2(e22, e22))) | ~ spl18_150), inference(avatar_component_clause, [], [f1108])).
fof(f1521, plain, (~ spl18_205 | ~ spl18_206 | ~ spl18_207 | ~ spl18_208 | ~ spl18_209 | ~ spl18_210 | ~ spl18_211 | ~ spl18_212 | spl18_175 | spl18_171 | spl18_167 | ~ spl18_214 | ~ spl18_215 | ~ spl18_216 | ~ spl18_217 | ~ spl18_218 | ~ spl18_219 | ~ spl18_220 | ~ spl18_221), inference(avatar_split_clause, [], [f1520, f1506, f1502, f1498, f1494, f1490, f1486, f1482, f1478, f1233, f1253, f1273, f1470, f1466, f1462, f1458, f1454, f1450, f1446, f1442])).
fof(f1273, plain, (spl18_175 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl18_175])])).
fof(f1253, plain, (spl18_171 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl18_171])])).
fof(f1233, plain, (spl18_167 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl18_167])])).
fof(f1520, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e23)) | ~ (h4(e13) = h4(op1(e11, e10))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), e23)) | ~ (h4(e11) = h4(op1(e12, e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e10) = h4(op1(e12, e12))) | ~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1519, f330])).
fof(f1519, plain, (~ (h4(e13) = h4(op1(e11, e10))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), e23)) | ~ (h4(e11) = h4(op1(e12, e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e10) = h4(op1(e12, e12))) | ~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1518, f1231])).
fof(f1518, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e23)) | ~ (h4(e11) = h4(op1(e12, e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e10) = h4(op1(e12, e12))) | ~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1517, f330])).
fof(f1517, plain, (~ (h4(e11) = h4(op1(e12, e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e10) = h4(op1(e12, e12))) | ~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1516, f1230])).
fof(f1516, plain, (~ (h4(op1(e12, e10)) = op2(e23, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e10) = h4(op1(e12, e12))) | ~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1515, f330])).
fof(f1515, plain, (~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e10) = h4(op1(e12, e12))) | ~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1514, f330])).
fof(f1514, plain, (~ (h4(e10) = h4(op1(e12, e12))) | ~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1513, f331])).
fof(f1513, plain, (~ (op2(e23, e23) = h4(op1(e12, e12))) | ~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1512, f330])).
fof(f1512, plain, (~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1511, f330])).
fof(f1511, plain, (~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1510, f330])).
fof(f1510, plain, (sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f396, f330])).
fof(f396, plain, (~ (e23 = h4(e12)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP14 | sP13 | sP12 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP8 | sP7 | sP6 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29])).
fof(f29, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP6), inference(usedef, [], [e29])).
fof(e29, plain, (sP6 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f30, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP7), inference(usedef, [], [e30])).
fof(e30, plain, (sP7 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f31, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP8), inference(usedef, [], [e31])).
fof(e31, plain, (sP8 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f32, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP9), inference(usedef, [], [e32])).
fof(e32, plain, (sP9 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f33, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP10), inference(usedef, [], [e33])).
fof(e33, plain, (sP10 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f34, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP11), inference(usedef, [], [e34])).
fof(e34, plain, (sP11 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f35, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP12), inference(usedef, [], [e35])).
fof(e35, plain, (sP12 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f36, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP13), inference(usedef, [], [e36])).
fof(e36, plain, (sP13 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f37, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP14), inference(usedef, [], [e37])).
fof(e37, plain, (sP14 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f38, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP15), inference(usedef, [], [e38])).
fof(e38, plain, (sP15 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f39, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP16), inference(usedef, [], [e39])).
fof(e39, plain, (sP16 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f40, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP17), inference(usedef, [], [e40])).
fof(e40, plain, (sP17 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', co1)).
fof(f1291, plain, (~ spl18_175 | ~ spl18_178), inference(avatar_split_clause, [], [f342, f1288, f1273])).
fof(f342, plain, (~ (e20 = h4(e10)) | ~ sP15), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP15), inference(nnf_transformation, [], [f38])).
fof(f1260, plain, (~ spl18_171 | ~ spl18_172), inference(avatar_split_clause, [], [f341, f1257, f1253])).
fof(f341, plain, (~ (e21 = h4(e13)) | ~ sP16), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP16), inference(nnf_transformation, [], [f39])).
fof(f1246, plain, (~ spl18_167 | ~ spl18_169), inference(avatar_split_clause, [], [f335, f1243, f1233])).
fof(f335, plain, (~ (e22 = h4(e11)) | ~ sP17), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP17), inference(nnf_transformation, [], [f40])).
fof(f1193, plain, spl18_85, inference(avatar_split_clause, [], [f315, f788])).
fof(f315, plain, (e20 = op2(e22, e22)), inference(cnf_transformation, [], [f13])).
fof(f1191, plain, spl18_21, inference(avatar_split_clause, [], [f312, f484])).
fof(f312, plain, (e10 = op1(e12, e12)), inference(cnf_transformation, [], [f12])).
fof(f1189, plain, (spl18_158 | spl18_153 | spl18_148 | spl18_166), inference(avatar_split_clause, [], [f308, f1186, f1099, f1123, f1147])).
fof(f1147, plain, (spl18_158 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl18_158])])).
fof(f1123, plain, (spl18_153 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl18_153])])).
fof(f1099, plain, (spl18_148 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl18_148])])).
fof(f308, plain, ((e20 = op2(e20, op2(e23, e20))) | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f28])).
fof(f28, plain, (((e23 = op2(e23, op2(e23, e23))) & (e22 = op2(e22, op2(e23, e22))) & (e21 = op2(e21, op2(e23, e21))) & (e20 = op2(e20, op2(e23, e20)))) | sP5 | sP4 | sP3), inference(definition_folding, [], [f11, e27, e26, e25])).
fof(f25, plain, (((e23 = op2(e23, op2(e20, e23))) & (e22 = op2(e22, op2(e20, e22))) & (e21 = op2(e21, op2(e20, e21))) & (e20 = op2(e20, op2(e20, e20)))) | ~ sP3), inference(usedef, [], [e25])).
fof(e25, plain, (sP3 <=> ((e23 = op2(e23, op2(e20, e23))) & (e22 = op2(e22, op2(e20, e22))) & (e21 = op2(e21, op2(e20, e21))) & (e20 = op2(e20, op2(e20, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f26, plain, (((e23 = op2(e23, op2(e21, e23))) & (e22 = op2(e22, op2(e21, e22))) & (e21 = op2(e21, op2(e21, e21))) & (e20 = op2(e20, op2(e21, e20)))) | ~ sP4), inference(usedef, [], [e26])).
fof(e26, plain, (sP4 <=> ((e23 = op2(e23, op2(e21, e23))) & (e22 = op2(e22, op2(e21, e22))) & (e21 = op2(e21, op2(e21, e21))) & (e20 = op2(e20, op2(e21, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f27, plain, (((e23 = op2(e23, op2(e22, e23))) & (e22 = op2(e22, op2(e22, e22))) & (e21 = op2(e21, op2(e22, e21))) & (e20 = op2(e20, op2(e22, e20)))) | ~ sP5), inference(usedef, [], [e27])).
fof(e27, plain, (sP5 <=> ((e23 = op2(e23, op2(e22, e23))) & (e22 = op2(e22, op2(e22, e22))) & (e21 = op2(e21, op2(e22, e21))) & (e20 = op2(e20, op2(e22, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f11, plain, (((e23 = op2(e23, op2(e23, e23))) & (e22 = op2(e22, op2(e23, e22))) & (e21 = op2(e21, op2(e23, e21))) & (e20 = op2(e20, op2(e23, e20)))) | ((e23 = op2(e23, op2(e22, e23))) & (e22 = op2(e22, op2(e22, e22))) & (e21 = op2(e21, op2(e22, e21))) & (e20 = op2(e20, op2(e22, e20)))) | ((e23 = op2(e23, op2(e21, e23))) & (e22 = op2(e22, op2(e21, e22))) & (e21 = op2(e21, op2(e21, e21))) & (e20 = op2(e20, op2(e21, e20)))) | ((e23 = op2(e23, op2(e20, e23))) & (e22 = op2(e22, op2(e20, e22))) & (e21 = op2(e21, op2(e20, e21))) & (e20 = op2(e20, op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax11)).
fof(f1164, plain, (~ spl18_158 | spl18_161), inference(avatar_split_clause, [], [f305, f1161, f1147])).
fof(f305, plain, ((e21 = op2(e21, op2(e20, e21))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (((e23 = op2(e23, op2(e20, e23))) & (e22 = op2(e22, op2(e20, e22))) & (e21 = op2(e21, op2(e20, e21))) & (e20 = op2(e20, op2(e20, e20)))) | ~ sP3), inference(nnf_transformation, [], [f25])).
fof(f1159, plain, (~ spl18_158 | spl18_160), inference(avatar_split_clause, [], [f306, f1156, f1147])).
fof(f306, plain, ((e22 = op2(e22, op2(e20, e22))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f1154, plain, (~ spl18_158 | spl18_159), inference(avatar_split_clause, [], [f307, f1151, f1147])).
fof(f307, plain, ((e23 = op2(e23, op2(e20, e23))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f1140, plain, (~ spl18_153 | spl18_156), inference(avatar_split_clause, [], [f301, f1137, f1123])).
fof(f301, plain, ((e21 = op2(e21, op2(e21, e21))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (((e23 = op2(e23, op2(e21, e23))) & (e22 = op2(e22, op2(e21, e22))) & (e21 = op2(e21, op2(e21, e21))) & (e20 = op2(e20, op2(e21, e20)))) | ~ sP4), inference(nnf_transformation, [], [f26])).
fof(f1130, plain, (~ spl18_153 | spl18_154), inference(avatar_split_clause, [], [f303, f1127, f1123])).
fof(f303, plain, ((e23 = op2(e23, op2(e21, e23))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f1111, plain, (~ spl18_148 | spl18_150), inference(avatar_split_clause, [], [f298, f1108, f1099])).
fof(f298, plain, ((e22 = op2(e22, op2(e22, e22))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (((e23 = op2(e23, op2(e22, e23))) & (e22 = op2(e22, op2(e22, e22))) & (e21 = op2(e21, op2(e22, e21))) & (e20 = op2(e20, op2(e22, e20)))) | ~ sP5), inference(nnf_transformation, [], [f27])).
fof(f1097, plain, (spl18_139 | spl18_134 | spl18_129 | spl18_147), inference(avatar_split_clause, [], [f292, f1094, f1007, f1031, f1055])).
fof(f1055, plain, (spl18_139 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl18_139])])).
fof(f1031, plain, (spl18_134 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl18_134])])).
fof(f1007, plain, (spl18_129 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl18_129])])).
fof(f292, plain, ((e10 = op1(e10, op1(e13, e10))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f24])).
fof(f24, plain, (((e13 = op1(e13, op1(e13, e13))) & (e12 = op1(e12, op1(e13, e12))) & (e11 = op1(e11, op1(e13, e11))) & (e10 = op1(e10, op1(e13, e10)))) | sP2 | sP1 | sP0), inference(definition_folding, [], [f10, e23, e22, e21])).
fof(f21, plain, (((e13 = op1(e13, op1(e10, e13))) & (e12 = op1(e12, op1(e10, e12))) & (e11 = op1(e11, op1(e10, e11))) & (e10 = op1(e10, op1(e10, e10)))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> ((e13 = op1(e13, op1(e10, e13))) & (e12 = op1(e12, op1(e10, e12))) & (e11 = op1(e11, op1(e10, e11))) & (e10 = op1(e10, op1(e10, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, (((e13 = op1(e13, op1(e11, e13))) & (e12 = op1(e12, op1(e11, e12))) & (e11 = op1(e11, op1(e11, e11))) & (e10 = op1(e10, op1(e11, e10)))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> ((e13 = op1(e13, op1(e11, e13))) & (e12 = op1(e12, op1(e11, e12))) & (e11 = op1(e11, op1(e11, e11))) & (e10 = op1(e10, op1(e11, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, (((e13 = op1(e13, op1(e12, e13))) & (e12 = op1(e12, op1(e12, e12))) & (e11 = op1(e11, op1(e12, e11))) & (e10 = op1(e10, op1(e12, e10)))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> ((e13 = op1(e13, op1(e12, e13))) & (e12 = op1(e12, op1(e12, e12))) & (e11 = op1(e11, op1(e12, e11))) & (e10 = op1(e10, op1(e12, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f10, plain, (((e13 = op1(e13, op1(e13, e13))) & (e12 = op1(e12, op1(e13, e12))) & (e11 = op1(e11, op1(e13, e11))) & (e10 = op1(e10, op1(e13, e10)))) | ((e13 = op1(e13, op1(e12, e13))) & (e12 = op1(e12, op1(e12, e12))) & (e11 = op1(e11, op1(e12, e11))) & (e10 = op1(e10, op1(e12, e10)))) | ((e13 = op1(e13, op1(e11, e13))) & (e12 = op1(e12, op1(e11, e12))) & (e11 = op1(e11, op1(e11, e11))) & (e10 = op1(e10, op1(e11, e10)))) | ((e13 = op1(e13, op1(e10, e13))) & (e12 = op1(e12, op1(e10, e12))) & (e11 = op1(e11, op1(e10, e11))) & (e10 = op1(e10, op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax10)).
fof(f1077, plain, (~ spl18_139 | spl18_143), inference(avatar_split_clause, [], [f288, f1074, f1055])).
fof(f288, plain, ((e10 = op1(e10, op1(e10, e10))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (((e13 = op1(e13, op1(e10, e13))) & (e12 = op1(e12, op1(e10, e12))) & (e11 = op1(e11, op1(e10, e11))) & (e10 = op1(e10, op1(e10, e10)))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1072, plain, (~ spl18_139 | spl18_142), inference(avatar_split_clause, [], [f289, f1069, f1055])).
fof(f289, plain, ((e11 = op1(e11, op1(e10, e11))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f1067, plain, (~ spl18_139 | spl18_141), inference(avatar_split_clause, [], [f290, f1064, f1055])).
fof(f290, plain, ((e12 = op1(e12, op1(e10, e12))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f1062, plain, (~ spl18_139 | spl18_140), inference(avatar_split_clause, [], [f291, f1059, f1055])).
fof(f291, plain, ((e13 = op1(e13, op1(e10, e13))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f1048, plain, (~ spl18_134 | spl18_137), inference(avatar_split_clause, [], [f285, f1045, f1031])).
fof(f285, plain, ((e11 = op1(e11, op1(e11, e11))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (((e13 = op1(e13, op1(e11, e13))) & (e12 = op1(e12, op1(e11, e12))) & (e11 = op1(e11, op1(e11, e11))) & (e10 = op1(e10, op1(e11, e10)))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1038, plain, (~ spl18_134 | spl18_135), inference(avatar_split_clause, [], [f287, f1035, f1031])).
fof(f287, plain, ((e13 = op1(e13, op1(e11, e13))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f1019, plain, (~ spl18_129 | spl18_131), inference(avatar_split_clause, [], [f282, f1016, f1007])).
fof(f282, plain, ((e12 = op1(e12, op1(e12, e12))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (((e13 = op1(e13, op1(e12, e13))) & (e12 = op1(e12, op1(e12, e12))) & (e11 = op1(e11, op1(e12, e11))) & (e10 = op1(e10, op1(e12, e10)))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1005, plain, (spl18_125 | spl18_121 | spl18_117 | spl18_113), inference(avatar_split_clause, [], [f124, f907, f924, f941, f958])).
fof(f124, plain, ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax4)).
fof(f1004, plain, (spl18_125 | spl18_109 | spl18_93 | spl18_77), inference(avatar_split_clause, [], [f125, f754, f822, f890, f958])).
fof(f125, plain, ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f1001, plain, (spl18_127 | spl18_123 | spl18_119 | spl18_115), inference(avatar_split_clause, [], [f128, f915, f932, f949, f966])).
fof(f128, plain, ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1000, plain, (spl18_127 | spl18_111 | spl18_95 | spl18_79), inference(avatar_split_clause, [], [f129, f762, f830, f898, f966])).
fof(f129, plain, ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f999, plain, (spl18_128 | spl18_124 | spl18_120 | spl18_116), inference(avatar_split_clause, [], [f130, f919, f936, f953, f970])).
fof(f130, plain, ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f997, plain, (spl18_109 | spl18_105 | spl18_101 | spl18_97), inference(avatar_split_clause, [], [f132, f839, f856, f873, f890])).
fof(f132, plain, ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f984, plain, (spl18_119 | spl18_103 | spl18_87 | spl18_71), inference(avatar_split_clause, [], [f145, f728, f796, f864, f932])).
fof(f145, plain, ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f981, plain, (spl18_77 | spl18_73 | spl18_69 | spl18_65), inference(avatar_split_clause, [], [f148, f703, f720, f737, f754])).
fof(f148, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f980, plain, (spl18_113 | spl18_97 | spl18_81 | spl18_65), inference(avatar_split_clause, [], [f149, f703, f771, f839, f907])).
fof(f149, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f979, plain, (spl18_78 | spl18_74 | spl18_70 | spl18_66), inference(avatar_split_clause, [], [f150, f707, f724, f741, f758])).
fof(f150, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f974, plain, (spl18_116 | spl18_100 | spl18_84 | spl18_68), inference(avatar_split_clause, [], [f155, f715, f783, f851, f919])).
fof(f155, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f939, plain, (spl18_117 | spl18_118 | spl18_119 | spl18_120), inference(avatar_split_clause, [], [f110, f936, f932, f928, f924])).
fof(f110, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax3)).
fof(f854, plain, (spl18_97 | spl18_98 | spl18_99 | spl18_100), inference(avatar_split_clause, [], [f115, f851, f847, f843, f839])).
fof(f115, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f820, plain, (spl18_89 | spl18_90 | spl18_91 | spl18_92), inference(avatar_split_clause, [], [f117, f817, f813, f809, f805])).
fof(f117, plain, ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f3])).
fof(f769, plain, (spl18_77 | spl18_78 | spl18_79 | spl18_80), inference(avatar_split_clause, [], [f120, f766, f762, f758, f754])).
fof(f120, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f3])).
fof(f735, plain, (spl18_69 | spl18_70 | spl18_71 | spl18_72), inference(avatar_split_clause, [], [f122, f732, f728, f724, f720])).
fof(f122, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f697, plain, (spl18_63 | spl18_59 | spl18_55 | spl18_51), inference(avatar_split_clause, [], [f80, f611, f628, f645, f662])).
fof(f80, plain, ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax2)).
fof(f695, plain, (spl18_64 | spl18_60 | spl18_56 | spl18_52), inference(avatar_split_clause, [], [f82, f615, f632, f649, f666])).
fof(f82, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f693, plain, (spl18_45 | spl18_41 | spl18_37 | spl18_33), inference(avatar_split_clause, [], [f84, f535, f552, f569, f586])).
fof(f84, plain, ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f690, plain, (spl18_58 | spl18_42 | spl18_26 | spl18_10), inference(avatar_split_clause, [], [f87, f437, f505, f573, f641])).
fof(f87, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f688, plain, (spl18_59 | spl18_43 | spl18_27 | spl18_11), inference(avatar_split_clause, [], [f89, f441, f509, f577, f645])).
fof(f89, plain, ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f680, plain, (spl18_55 | spl18_39 | spl18_23 | spl18_7), inference(avatar_split_clause, [], [f97, f424, f492, f560, f628])).
fof(f97, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f676, plain, (spl18_49 | spl18_33 | spl18_17 | spl18_1), inference(avatar_split_clause, [], [f101, f399, f467, f535, f603])).
fof(f101, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f669, plain, (spl18_61 | spl18_62 | spl18_63 | spl18_64), inference(avatar_split_clause, [], [f60, f666, f662, f658, f654])).
fof(f60, plain, ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG107+1.p', ax1)).
fof(f635, plain, (spl18_53 | spl18_54 | spl18_55 | spl18_56), inference(avatar_split_clause, [], [f62, f632, f628, f624, f620])).
fof(f62, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f584, plain, (spl18_41 | spl18_42 | spl18_43 | spl18_44), inference(avatar_split_clause, [], [f65, f581, f577, f573, f569])).
fof(f65, plain, ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f482, plain, (spl18_17 | spl18_18 | spl18_19 | spl18_20), inference(avatar_split_clause, [], [f71, f479, f475, f471, f467])).
fof(f71, plain, ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f465, plain, (spl18_13 | spl18_14 | spl18_15 | spl18_16), inference(avatar_split_clause, [], [f72, f462, f458, f454, f450])).
fof(f72, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).