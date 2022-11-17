fof(f3504, plain, $false, inference(avatar_sat_refutation, [], [f1416, f1420, f1421, f1423, f1432, f1437, f1450, f1467, f1468, f1512, f1691, f1775, f1874, f2083, f2127, f2190, f2219, f2254, f2327, f2339, f2341, f2345, f2348, f2353, f2355, f2378, f2381, f2383, f2468, f2571, f2608, f2638, f2650, f2694, f2740, f2774, f2806, f2813, f2836, f2864, f2897, f2909, f2921, f2923, f2925, f2938, f2975, f2980, f3092, f3125, f3138, f3143, f3252, f3296, f3343, f3389, f3412, f3429, f3430, f3450, f3500])).
fof(f3500, plain, (~ spl130_12 | ~ spl130_15), inference(avatar_contradiction_clause, [], [f3499])).
fof(f3499, plain, ($false | (~ spl130_12 | ~ spl130_15)), inference(subsumption_resolution, [], [f3498, f463])).
fof(f463, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG066+1.p', ax5)).
fof(f3498, plain, ((e1 = e4) | (~ spl130_12 | ~ spl130_15)), inference(forward_demodulation, [], [f932, f920])).
fof(f920, plain, ((e1 = op(e4, e2)) | ~ spl130_12), inference(avatar_component_clause, [], [f918])).
fof(f918, plain, (spl130_12 <=> (e1 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_12])])).
fof(f932, plain, ((e4 = op(e4, e2)) | ~ spl130_15), inference(avatar_component_clause, [], [f930])).
fof(f930, plain, (spl130_15 <=> (e4 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_15])])).
fof(f3450, plain, (~ spl130_97 | ~ spl130_99), inference(avatar_contradiction_clause, [], [f3449])).
fof(f3449, plain, ($false | (~ spl130_97 | ~ spl130_99)), inference(subsumption_resolution, [], [f3448, f462])).
fof(f462, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f3448, plain, ((e1 = e3) | (~ spl130_97 | ~ spl130_99)), inference(forward_demodulation, [], [f1285, f1277])).
fof(f1277, plain, ((e1 = op(e1, e0)) | ~ spl130_97), inference(avatar_component_clause, [], [f1275])).
fof(f1275, plain, (spl130_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl130_97])])).
fof(f1285, plain, ((e3 = op(e1, e0)) | ~ spl130_99), inference(avatar_component_clause, [], [f1283])).
fof(f1283, plain, (spl130_99 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl130_99])])).
fof(f3430, plain, (spl130_105 | ~ spl130_126), inference(avatar_split_clause, [], [f3422, f1397, f1308])).
fof(f1308, plain, (spl130_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_105])])).
fof(f1397, plain, (spl130_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl130_126])])).
fof(f3422, plain, ((e4 = op(e0, e4)) | ~ spl130_126), inference(backward_demodulation, [], [f304, f1399])).
fof(f1399, plain, ((e0 = unit) | ~ spl130_126), inference(avatar_component_clause, [], [f1397])).
fof(f304, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG066+1.p', ax2)).
fof(f3429, plain, (spl130_109 | ~ spl130_126), inference(avatar_split_clause, [], [f3420, f1397, f1325])).
fof(f1325, plain, (spl130_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl130_109])])).
fof(f3420, plain, ((e3 = op(e0, e3)) | ~ spl130_126), inference(backward_demodulation, [], [f302, f1399])).
fof(f302, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f3412, plain, (~ spl130_109 | ~ spl130_9), inference(avatar_split_clause, [], [f3411, f905, f1325])).
fof(f905, plain, (spl130_9 <=> (e3 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl130_9])])).
fof(f3411, plain, (~ (e3 = op(e0, e3)) | ~ spl130_9), inference(forward_demodulation, [], [f390, f907])).
fof(f907, plain, ((e3 = op(e4, e3)) | ~ spl130_9), inference(avatar_component_clause, [], [f905])).
fof(f390, plain, ~ (op(e0, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG066+1.p', ax4)).
fof(f3389, plain, (~ spl130_39 | ~ spl130_49), inference(avatar_split_clause, [], [f3388, f1073, f1031])).
fof(f1031, plain, (spl130_39 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_39])])).
fof(f1073, plain, (spl130_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl130_49])])).
fof(f3388, plain, (~ (e3 = op(e3, e2)) | ~ spl130_49), inference(forward_demodulation, [], [f438, f1075])).
fof(f1075, plain, ((e3 = op(e3, e0)) | ~ spl130_49), inference(avatar_component_clause, [], [f1073])).
fof(f438, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f3343, plain, (spl130_82 | ~ spl130_129), inference(avatar_contradiction_clause, [], [f3342])).
fof(f3342, plain, ($false | (spl130_82 | ~ spl130_129)), inference(subsumption_resolution, [], [f3330, f1213])).
fof(f1213, plain, (~ (e1 = op(e1, e3)) | spl130_82), inference(avatar_component_clause, [], [f1212])).
fof(f1212, plain, (spl130_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl130_82])])).
fof(f3330, plain, ((e1 = op(e1, e3)) | ~ spl130_129), inference(backward_demodulation, [], [f299, f1411])).
fof(f1411, plain, ((e3 = unit) | ~ spl130_129), inference(avatar_component_clause, [], [f1409])).
fof(f1409, plain, (spl130_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl130_129])])).
fof(f299, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f3296, plain, (~ spl130_24 | ~ spl130_25), inference(avatar_contradiction_clause, [], [f3295])).
fof(f3295, plain, ($false | (~ spl130_24 | ~ spl130_25)), inference(subsumption_resolution, [], [f3294, f466])).
fof(f466, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f3294, plain, ((e3 = e4) | (~ spl130_24 | ~ spl130_25)), inference(backward_demodulation, [], [f974, f970])).
fof(f970, plain, ((e3 = op(e4, e0)) | ~ spl130_24), inference(avatar_component_clause, [], [f968])).
fof(f968, plain, (spl130_24 <=> (e3 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl130_24])])).
fof(f974, plain, ((e4 = op(e4, e0)) | ~ spl130_25), inference(avatar_component_clause, [], [f972])).
fof(f972, plain, (spl130_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl130_25])])).
fof(f3252, plain, (~ spl130_12 | ~ spl130_130), inference(avatar_contradiction_clause, [], [f3251])).
fof(f3251, plain, ($false | (~ spl130_12 | ~ spl130_130)), inference(subsumption_resolution, [], [f3250, f461])).
fof(f461, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f3250, plain, ((e1 = e2) | (~ spl130_12 | ~ spl130_130)), inference(forward_demodulation, [], [f3236, f920])).
fof(f3236, plain, ((e2 = op(e4, e2)) | ~ spl130_130), inference(backward_demodulation, [], [f300, f1415])).
fof(f1415, plain, ((e4 = unit) | ~ spl130_130), inference(avatar_component_clause, [], [f1413])).
fof(f1413, plain, (spl130_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl130_130])])).
fof(f300, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f3143, plain, (~ spl130_89 | ~ spl130_90), inference(avatar_contradiction_clause, [], [f3142])).
fof(f3142, plain, ($false | (~ spl130_89 | ~ spl130_90)), inference(subsumption_resolution, [], [f3141, f466])).
fof(f3141, plain, ((e3 = e4) | (~ spl130_89 | ~ spl130_90)), inference(forward_demodulation, [], [f1247, f1243])).
fof(f1243, plain, ((e3 = op(e1, e2)) | ~ spl130_89), inference(avatar_component_clause, [], [f1241])).
fof(f1241, plain, (spl130_89 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_89])])).
fof(f1247, plain, ((e4 = op(e1, e2)) | ~ spl130_90), inference(avatar_component_clause, [], [f1245])).
fof(f1245, plain, (spl130_90 <=> (e4 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_90])])).
fof(f3138, plain, (~ spl130_102 | ~ spl130_105), inference(avatar_contradiction_clause, [], [f3137])).
fof(f3137, plain, ($false | (~ spl130_102 | ~ spl130_105)), inference(subsumption_resolution, [], [f3135, f463])).
fof(f3135, plain, ((e1 = e4) | (~ spl130_102 | ~ spl130_105)), inference(backward_demodulation, [], [f1310, f1298])).
fof(f1298, plain, ((e1 = op(e0, e4)) | ~ spl130_102), inference(avatar_component_clause, [], [f1296])).
fof(f1296, plain, (spl130_102 <=> (e1 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_102])])).
fof(f1310, plain, ((e4 = op(e0, e4)) | ~ spl130_105), inference(avatar_component_clause, [], [f1308])).
fof(f3125, plain, (spl130_73 | ~ spl130_126), inference(avatar_split_clause, [], [f3116, f1397, f1174])).
fof(f1174, plain, (spl130_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl130_73])])).
fof(f3116, plain, ((e2 = op(e2, e0)) | ~ spl130_126), inference(backward_demodulation, [], [f301, f1399])).
fof(f301, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f3092, plain, (~ spl130_78 | ~ spl130_83), inference(avatar_split_clause, [], [f2922, f1216, f1195])).
fof(f1195, plain, (spl130_78 <=> (e2 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_78])])).
fof(f1216, plain, (spl130_83 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl130_83])])).
fof(f2922, plain, (~ (e2 = op(e1, e4)) | ~ spl130_83), inference(forward_demodulation, [], [f426, f1218])).
fof(f1218, plain, ((e2 = op(e1, e3)) | ~ spl130_83), inference(avatar_component_clause, [], [f1216])).
fof(f426, plain, ~ (op(e1, e3) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f2980, plain, (~ spl130_38 | ~ spl130_113), inference(avatar_split_clause, [], [f2979, f1342, f1027])).
fof(f1027, plain, (spl130_38 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_38])])).
fof(f1342, plain, (spl130_113 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_113])])).
fof(f2979, plain, (~ (e2 = op(e3, e2)) | ~ spl130_113), inference(backward_demodulation, [], [f379, f1344])).
fof(f1344, plain, ((e2 = op(e0, e2)) | ~ spl130_113), inference(avatar_component_clause, [], [f1342])).
fof(f379, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f2975, plain, (~ spl130_12 | ~ spl130_128), inference(avatar_contradiction_clause, [], [f2974])).
fof(f2974, plain, ($false | (~ spl130_12 | ~ spl130_128)), inference(subsumption_resolution, [], [f2973, f463])).
fof(f2973, plain, ((e1 = e4) | (~ spl130_12 | ~ spl130_128)), inference(forward_demodulation, [], [f2950, f920])).
fof(f2950, plain, ((e4 = op(e4, e2)) | ~ spl130_128), inference(backward_demodulation, [], [f305, f1407])).
fof(f1407, plain, ((e2 = unit) | ~ spl130_128), inference(avatar_component_clause, [], [f1405])).
fof(f1405, plain, (spl130_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl130_128])])).
fof(f305, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2938, plain, (~ spl130_115 | ~ spl130_105), inference(avatar_split_clause, [], [f2937, f1308, f1350])).
fof(f1350, plain, (spl130_115 <=> (e4 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_115])])).
fof(f2937, plain, (~ (e4 = op(e0, e2)) | ~ spl130_105), inference(backward_demodulation, [], [f415, f1310])).
fof(f415, plain, ~ (op(e0, e2) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f2925, plain, (spl130_91 | ~ spl130_12), inference(avatar_split_clause, [], [f2673, f918, f1250])).
fof(f1250, plain, (spl130_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl130_91])])).
fof(f2673, plain, ((e0 = op(e1, e1)) | ~ spl130_12), inference(forward_demodulation, [], [f467, f920])).
fof(f467, plain, (e0 = op(op(e4, e2), op(e4, e2))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e3 = op(e2, e4)) & (e1 = op(e4, e2)) & (e0 = op(op(e4, e2), op(e4, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG066+1.p', ax6)).
fof(f2923, plain, (~ spl130_79 | ~ spl130_54), inference(avatar_split_clause, [], [f2720, f1094, f1199])).
fof(f1199, plain, (spl130_79 <=> (e3 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_79])])).
fof(f1094, plain, (spl130_54 <=> (e3 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_54])])).
fof(f2720, plain, (~ (e3 = op(e1, e4)) | ~ spl130_54), inference(forward_demodulation, [], [f401, f1096])).
fof(f1096, plain, ((e3 = op(e2, e4)) | ~ spl130_54), inference(avatar_component_clause, [], [f1094])).
fof(f401, plain, ~ (op(e1, e4) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f2921, plain, (~ spl130_77 | ~ spl130_97), inference(avatar_split_clause, [], [f2920, f1275, f1191])).
fof(f1191, plain, (spl130_77 <=> (e1 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_77])])).
fof(f2920, plain, (~ (e1 = op(e1, e4)) | ~ spl130_97), inference(forward_demodulation, [], [f420, f1277])).
fof(f420, plain, ~ (op(e1, e0) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f2909, plain, (~ spl130_4 | ~ spl130_54), inference(avatar_split_clause, [], [f2908, f1094, f884])).
fof(f884, plain, (spl130_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_4])])).
fof(f2908, plain, (~ (e3 = op(e4, e4)) | ~ spl130_54), inference(backward_demodulation, [], [f405, f1096])).
fof(f405, plain, ~ (op(e2, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2897, plain, (~ spl130_73 | ~ spl130_48), inference(avatar_split_clause, [], [f2896, f1069, f1174])).
fof(f1069, plain, (spl130_48 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl130_48])])).
fof(f2896, plain, (~ (e2 = op(e2, e0)) | ~ spl130_48), inference(forward_demodulation, [], [f364, f1071])).
fof(f1071, plain, ((e2 = op(e3, e0)) | ~ spl130_48), inference(avatar_component_clause, [], [f1069])).
fof(f364, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f2864, plain, (~ spl130_2 | ~ spl130_12), inference(avatar_split_clause, [], [f2863, f918, f876])).
fof(f876, plain, (spl130_2 <=> (e1 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_2])])).
fof(f2863, plain, (~ (e1 = op(e4, e4)) | ~ spl130_12), inference(forward_demodulation, [], [f455, f920])).
fof(f455, plain, ~ (op(e4, e2) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2836, plain, (~ spl130_53 | ~ spl130_54), inference(avatar_contradiction_clause, [], [f2835])).
fof(f2835, plain, ($false | (~ spl130_53 | ~ spl130_54)), inference(subsumption_resolution, [], [f2834, f464])).
fof(f464, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f2834, plain, ((e2 = e3) | (~ spl130_53 | ~ spl130_54)), inference(backward_demodulation, [], [f1096, f1092])).
fof(f1092, plain, ((e2 = op(e2, e4)) | ~ spl130_53), inference(avatar_component_clause, [], [f1090])).
fof(f1090, plain, (spl130_53 <=> (e2 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_53])])).
fof(f2813, plain, (~ spl130_82 | ~ spl130_97), inference(avatar_split_clause, [], [f2810, f1275, f1212])).
fof(f2810, plain, (~ (e1 = op(e1, e3)) | ~ spl130_97), inference(backward_demodulation, [], [f419, f1277])).
fof(f419, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2806, plain, (~ spl130_84 | ~ spl130_109), inference(avatar_split_clause, [], [f2804, f1325, f1220])).
fof(f1220, plain, (spl130_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl130_84])])).
fof(f2804, plain, (~ (e3 = op(e1, e3)) | ~ spl130_109), inference(backward_demodulation, [], [f387, f1327])).
fof(f1327, plain, ((e3 = op(e0, e3)) | ~ spl130_109), inference(avatar_component_clause, [], [f1325])).
fof(f387, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2774, plain, (spl130_96 | ~ spl130_127), inference(avatar_split_clause, [], [f2761, f1401, f1271])).
fof(f1271, plain, (spl130_96 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl130_96])])).
fof(f1401, plain, (spl130_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl130_127])])).
fof(f2761, plain, ((e0 = op(e1, e0)) | ~ spl130_127), inference(backward_demodulation, [], [f296, f1403])).
fof(f1403, plain, ((e1 = unit) | ~ spl130_127), inference(avatar_component_clause, [], [f1401])).
fof(f296, plain, (e0 = op(unit, e0)), inference(cnf_transformation, [], [f2])).
fof(f2740, plain, (~ spl130_103 | ~ spl130_105), inference(avatar_contradiction_clause, [], [f2739])).
fof(f2739, plain, ($false | (~ spl130_103 | ~ spl130_105)), inference(subsumption_resolution, [], [f2738, f465])).
fof(f465, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f2738, plain, ((e2 = e4) | (~ spl130_103 | ~ spl130_105)), inference(forward_demodulation, [], [f1310, f1302])).
fof(f1302, plain, ((e2 = op(e0, e4)) | ~ spl130_103), inference(avatar_component_clause, [], [f1300])).
fof(f1300, plain, (spl130_103 <=> (e2 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_103])])).
fof(f2694, plain, (~ spl130_37 | ~ spl130_12), inference(avatar_split_clause, [], [f2693, f918, f1023])).
fof(f1023, plain, (spl130_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_37])])).
fof(f2693, plain, (~ (e1 = op(e3, e2)) | ~ spl130_12), inference(forward_demodulation, [], [f386, f920])).
fof(f386, plain, ~ (op(e3, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f2650, plain, (~ spl130_12 | ~ spl130_14), inference(avatar_contradiction_clause, [], [f2649])).
fof(f2649, plain, ($false | (~ spl130_12 | ~ spl130_14)), inference(subsumption_resolution, [], [f2648, f462])).
fof(f2648, plain, ((e1 = e3) | (~ spl130_12 | ~ spl130_14)), inference(backward_demodulation, [], [f928, f920])).
fof(f928, plain, ((e3 = op(e4, e2)) | ~ spl130_14), inference(avatar_component_clause, [], [f926])).
fof(f926, plain, (spl130_14 <=> (e3 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_14])])).
fof(f2638, plain, (~ spl130_18 | ~ spl130_19), inference(avatar_contradiction_clause, [], [f2637])).
fof(f2637, plain, ($false | (~ spl130_18 | ~ spl130_19)), inference(subsumption_resolution, [], [f2636, f464])).
fof(f2636, plain, ((e2 = e3) | (~ spl130_18 | ~ spl130_19)), inference(backward_demodulation, [], [f949, f945])).
fof(f945, plain, ((e2 = op(e4, e1)) | ~ spl130_18), inference(avatar_component_clause, [], [f943])).
fof(f943, plain, (spl130_18 <=> (e2 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl130_18])])).
fof(f949, plain, ((e3 = op(e4, e1)) | ~ spl130_19), inference(avatar_component_clause, [], [f947])).
fof(f947, plain, (spl130_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl130_19])])).
fof(f2608, plain, (~ spl130_27 | ~ spl130_28), inference(avatar_contradiction_clause, [], [f2607])).
fof(f2607, plain, ($false | (~ spl130_27 | ~ spl130_28)), inference(subsumption_resolution, [], [f2606, f461])).
fof(f2606, plain, ((e1 = e2) | (~ spl130_27 | ~ spl130_28)), inference(backward_demodulation, [], [f987, f983])).
fof(f983, plain, ((e1 = op(e3, e4)) | ~ spl130_27), inference(avatar_component_clause, [], [f981])).
fof(f981, plain, (spl130_27 <=> (e1 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_27])])).
fof(f987, plain, ((e2 = op(e3, e4)) | ~ spl130_28), inference(avatar_component_clause, [], [f985])).
fof(f985, plain, (spl130_28 <=> (e2 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_28])])).
fof(f2571, plain, (~ spl130_52 | ~ spl130_54), inference(avatar_contradiction_clause, [], [f2570])).
fof(f2570, plain, ($false | (~ spl130_52 | ~ spl130_54)), inference(subsumption_resolution, [], [f2569, f462])).
fof(f2569, plain, ((e1 = e3) | (~ spl130_52 | ~ spl130_54)), inference(backward_demodulation, [], [f1096, f1088])).
fof(f1088, plain, ((e1 = op(e2, e4)) | ~ spl130_52), inference(avatar_component_clause, [], [f1086])).
fof(f1086, plain, (spl130_52 <=> (e1 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_52])])).
fof(f2468, plain, (~ spl130_91 | ~ spl130_96), inference(avatar_split_clause, [], [f2459, f1271, f1250])).
fof(f2459, plain, (~ (e0 = op(e1, e1)) | ~ spl130_96), inference(backward_demodulation, [], [f417, f1273])).
fof(f1273, plain, ((e0 = op(e1, e0)) | ~ spl130_96), inference(avatar_component_clause, [], [f1271])).
fof(f417, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f2383, plain, (spl130_25 | ~ spl130_126), inference(avatar_split_clause, [], [f2369, f1397, f972])).
fof(f2369, plain, ((e4 = op(e4, e0)) | ~ spl130_126), inference(backward_demodulation, [], [f305, f1399])).
fof(f2381, plain, (spl130_49 | ~ spl130_126), inference(avatar_split_clause, [], [f2367, f1397, f1073])).
fof(f2367, plain, ((e3 = op(e3, e0)) | ~ spl130_126), inference(backward_demodulation, [], [f303, f1399])).
fof(f303, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f2378, plain, (spl130_113 | ~ spl130_126), inference(avatar_split_clause, [], [f2364, f1397, f1342])).
fof(f2364, plain, ((e2 = op(e0, e2)) | ~ spl130_126), inference(backward_demodulation, [], [f300, f1399])).
fof(f2355, plain, (~ spl130_91 | spl130_97), inference(avatar_split_clause, [], [f850, f1275, f1250])).
fof(f850, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ((sP129 | sP128 | sP127 | sP126 | sP125) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(definition_folding, [], [f9, e139, e138, e137, e136, e135, e134, e133, e132, e131, e130, e129, e128, e127, e126, e125, e124, e123, e122, e121, e120, e119, e118, e117, e116, e115, e114, e113, e112, e111, e110, e109, e108, e107, e106, e105, e104, e103, e102, e101, e100, e99, e98, e97, e96, e95, e94, e93, e92, e91, e90, e89, e88, e87, e86, e85, e84, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
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
fof(f9, plain, (((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0))))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0))))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0))))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG066+1.p', co1)).
fof(f2353, plain, (~ spl130_94 | spl130_82), inference(avatar_split_clause, [], [f853, f1212, f1262])).
fof(f1262, plain, (spl130_94 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl130_94])])).
fof(f853, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f140])).
fof(f2348, plain, (~ spl130_65 | spl130_53), inference(avatar_split_clause, [], [f859, f1090, f1140])).
fof(f1140, plain, (spl130_65 <=> (e4 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_65])])).
fof(f859, plain, ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))), inference(cnf_transformation, [], [f140])).
fof(f2345, plain, (~ spl130_33 | spl130_39), inference(avatar_split_clause, [], [f862, f1031, f1006])).
fof(f1006, plain, (spl130_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl130_33])])).
fof(f862, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f140])).
fof(f2341, plain, (~ spl130_3 | spl130_15), inference(avatar_split_clause, [], [f867, f930, f880])).
fof(f880, plain, (spl130_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl130_3])])).
fof(f867, plain, ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))), inference(cnf_transformation, [], [f140])).
fof(f2339, plain, (spl130_235 | spl130_209 | spl130_183 | spl130_157 | spl130_131), inference(avatar_split_clause, [], [f870, f1470, f1599, f1728, f1857, f1986])).
fof(f1986, plain, (spl130_235 <=> sP125), introduced(avatar_definition, [new_symbols(naming, [spl130_235])])).
fof(f1857, plain, (spl130_209 <=> sP126), introduced(avatar_definition, [new_symbols(naming, [spl130_209])])).
fof(f1728, plain, (spl130_183 <=> sP127), introduced(avatar_definition, [new_symbols(naming, [spl130_183])])).
fof(f1599, plain, (spl130_157 <=> sP128), introduced(avatar_definition, [new_symbols(naming, [spl130_157])])).
fof(f1470, plain, (spl130_131 <=> sP129), introduced(avatar_definition, [new_symbols(naming, [spl130_131])])).
fof(f870, plain, (sP129 | sP128 | sP127 | sP126 | sP125), inference(cnf_transformation, [], [f140])).
fof(f2327, plain, (~ spl130_254 | ~ spl130_91 | ~ spl130_126), inference(avatar_split_clause, [], [f832, f1397, f1250, f2080])).
fof(f2080, plain, (spl130_254 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl130_254])])).
fof(f832, plain, (~ (e0 = unit) | ~ (e0 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f264])).
fof(f264, plain, ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1)) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f2254, plain, (~ spl130_212 | ~ spl130_12 | spl130_18), inference(avatar_split_clause, [], [f749, f943, f918, f1871])).
fof(f1871, plain, (spl130_212 <=> sP47), introduced(avatar_definition, [new_symbols(naming, [spl130_212])])).
fof(f749, plain, ((e2 = op(e4, e1)) | ~ (e1 = op(e4, e2)) | ~ sP47), inference(cnf_transformation, [], [f223])).
fof(f223, plain, ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2)) | ~ sP47), inference(nnf_transformation, [], [f57])).
fof(f2219, plain, (~ spl130_192 | ~ spl130_43 | spl130_37), inference(avatar_split_clause, [], [f711, f1023, f1048, f1772])).
fof(f1772, plain, (spl130_192 <=> sP66), introduced(avatar_definition, [new_symbols(naming, [spl130_192])])).
fof(f1048, plain, (spl130_43 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl130_43])])).
fof(f711, plain, ((e1 = op(e3, e2)) | ~ (e2 = op(e3, e1)) | ~ sP66), inference(cnf_transformation, [], [f204])).
fof(f204, plain, ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1)) | ~ sP66), inference(nnf_transformation, [], [f76])).
fof(f2190, plain, (~ spl130_175 | ~ spl130_89 | spl130_83), inference(avatar_split_clause, [], [f679, f1216, f1241, f1688])).
fof(f1688, plain, (spl130_175 <=> sP82), introduced(avatar_definition, [new_symbols(naming, [spl130_175])])).
fof(f679, plain, ((e2 = op(e1, e3)) | ~ (e3 = op(e1, e2)) | ~ sP82), inference(cnf_transformation, [], [f188])).
fof(f188, plain, ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2)) | ~ sP82), inference(nnf_transformation, [], [f92])).
fof(f2127, plain, (~ spl130_139 | ~ spl130_40 | spl130_28), inference(avatar_split_clause, [], [f609, f985, f1035, f1509])).
fof(f1509, plain, (spl130_139 <=> sP117), introduced(avatar_definition, [new_symbols(naming, [spl130_139])])).
fof(f1035, plain, (spl130_40 <=> (e4 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl130_40])])).
fof(f609, plain, ((e2 = op(e3, e4)) | ~ (e4 = op(e3, e2)) | ~ sP117), inference(cnf_transformation, [], [f153])).
fof(f153, plain, ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2)) | ~ sP117), inference(nnf_transformation, [], [f127])).
fof(f2083, plain, (~ spl130_235 | spl130_254), inference(avatar_split_clause, [], [f576, f2080, f1986])).
fof(f576, plain, (sP6 | ~ sP125), inference(cnf_transformation, [], [f145])).
fof(f145, plain, ((sP24 & sP23 & sP22 & sP21 & sP20 & sP19 & sP18 & sP17 & sP16 & sP15 & sP14 & sP13 & sP12 & sP11 & sP10 & sP9 & sP8 & sP7 & sP6 & sP5 & sP4 & sP3 & sP2 & sP1 & sP0) | ~ sP125), inference(nnf_transformation, [], [f135])).
fof(f1874, plain, (~ spl130_209 | spl130_212), inference(avatar_split_clause, [], [f567, f1871, f1857])).
fof(f567, plain, (sP47 | ~ sP126), inference(cnf_transformation, [], [f144])).
fof(f144, plain, ((sP49 & sP48 & sP47 & sP46 & sP45 & sP44 & sP43 & sP42 & sP41 & sP40 & sP39 & sP38 & sP37 & sP36 & sP35 & sP34 & sP33 & sP32 & sP31 & sP30 & sP29 & sP28 & sP27 & sP26 & sP25) | ~ sP126), inference(nnf_transformation, [], [f136])).
fof(f1775, plain, (~ spl130_183 | spl130_192), inference(avatar_split_clause, [], [f536, f1772, f1728])).
fof(f536, plain, (sP66 | ~ sP127), inference(cnf_transformation, [], [f143])).
fof(f143, plain, ((sP74 & sP73 & sP72 & sP71 & sP70 & sP69 & sP68 & sP67 & sP66 & sP65 & sP64 & sP63 & sP62 & sP61 & sP60 & sP59 & sP58 & sP57 & sP56 & sP55 & sP54 & sP53 & sP52 & sP51 & sP50) | ~ sP127), inference(nnf_transformation, [], [f137])).
fof(f1691, plain, (~ spl130_157 | spl130_175), inference(avatar_split_clause, [], [f502, f1688, f1599])).
fof(f502, plain, (sP82 | ~ sP128), inference(cnf_transformation, [], [f142])).
fof(f142, plain, ((sP99 & sP98 & sP97 & sP96 & sP95 & sP94 & sP93 & sP92 & sP91 & sP90 & sP89 & sP88 & sP87 & sP86 & sP85 & sP84 & sP83 & sP82 & sP81 & sP80 & sP79 & sP78 & sP77 & sP76 & sP75) | ~ sP128), inference(nnf_transformation, [], [f138])).
fof(f1512, plain, (~ spl130_131 | spl130_139), inference(avatar_split_clause, [], [f487, f1509, f1470])).
fof(f487, plain, (sP117 | ~ sP129), inference(cnf_transformation, [], [f141])).
fof(f141, plain, ((sP124 & sP123 & sP122 & sP121 & sP120 & sP119 & sP118 & sP117 & sP116 & sP115 & sP114 & sP113 & sP112 & sP111 & sP110 & sP109 & sP108 & sP107 & sP106 & sP105 & sP104 & sP103 & sP102 & sP101 & sP100) | ~ sP129), inference(nnf_transformation, [], [f139])).
fof(f1468, plain, spl130_12, inference(avatar_split_clause, [], [f468, f918])).
fof(f468, plain, (e1 = op(e4, e2)), inference(cnf_transformation, [], [f6])).
fof(f1467, plain, spl130_54, inference(avatar_split_clause, [], [f469, f1094])).
fof(f469, plain, (e3 = op(e2, e4)), inference(cnf_transformation, [], [f6])).
fof(f1450, plain, (spl130_99 | spl130_94 | spl130_89 | spl130_84 | spl130_79), inference(avatar_split_clause, [], [f323, f1199, f1220, f1241, f1262, f1283])).
fof(f323, plain, ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG066+1.p', ax3)).
fof(f1437, plain, (spl130_115 | spl130_90 | spl130_65 | spl130_40 | spl130_15), inference(avatar_split_clause, [], [f336, f930, f1035, f1140, f1245, f1350])).
fof(f336, plain, ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))), inference(cnf_transformation, [], [f3])).
fof(f1432, plain, (spl130_48 | spl130_43 | spl130_38 | spl130_33 | spl130_28), inference(avatar_split_clause, [], [f341, f985, f1006, f1027, f1048, f1069])).
fof(f341, plain, ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f1423, plain, (spl130_102 | spl130_77 | spl130_52 | spl130_27 | spl130_2), inference(avatar_split_clause, [], [f350, f876, f981, f1086, f1191, f1296])).
fof(f350, plain, ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f1421, plain, (spl130_103 | spl130_78 | spl130_53 | spl130_28 | spl130_3), inference(avatar_split_clause, [], [f352, f880, f985, f1090, f1195, f1300])).
fof(f352, plain, ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f1420, plain, (spl130_24 | spl130_19 | spl130_14 | spl130_9 | spl130_4), inference(avatar_split_clause, [], [f353, f884, f905, f926, f947, f968])).
fof(f353, plain, ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f1416, plain, (spl130_126 | spl130_127 | spl130_128 | spl130_129 | spl130_130), inference(avatar_split_clause, [], [f306, f1413, f1409, f1405, f1401, f1397])).
fof(f306, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).