fof(f2696, plain, $false, inference(avatar_sat_refutation, [], [f719, f908, f913, f914, f924, f926, f929, f930, f936, f941, f942, f959, f960, f969, f993, f1007, f1036, f1050, f1062, f1071, f1090, f1119, f1133, f1143, f1160, f1174, f1198, f1212, f1222, f1242, f1254, f1273, f1288, f1303, f1318, f1327, f1344, f1351, f1353, f1357, f1358, f1360, f1365, f1367, f1386, f1387, f1388, f1389, f1472, f1565, f1568, f1607, f1616, f1639, f1712, f1766, f1799, f1821, f1830, f1836, f1837, f1838, f1841, f1901, f1938, f1942, f1950, f1988, f2070, f2104, f2125, f2160, f2178, f2199, f2236, f2254, f2273, f2311, f2314, f2331, f2332, f2333, f2352, f2402, f2415, f2422, f2431, f2443, f2459, f2462, f2473, f2485, f2497, f2501, f2513, f2524, f2535, f2548, f2554, f2561, f2579, f2609, f2617, f2669, f2686])).
fof(f2686, plain, (~ spl24_23 | ~ spl24_25), inference(avatar_contradiction_clause, [], [f2685])).
fof(f2685, plain, ($false | (~ spl24_23 | ~ spl24_25)), inference(subsumption_resolution, [], [f2684, f253])).
fof(f253, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG068+1.p', ax5)).
fof(f2684, plain, ((e2 = e4) | (~ spl24_23 | ~ spl24_25)), inference(backward_demodulation, [], [f466, f458])).
fof(f458, plain, ((e2 = op(e4, e0)) | ~ spl24_23), inference(avatar_component_clause, [], [f456])).
fof(f456, plain, (spl24_23 <=> (e2 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_23])])).
fof(f466, plain, ((e4 = op(e4, e0)) | ~ spl24_25), inference(avatar_component_clause, [], [f464])).
fof(f464, plain, (spl24_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_25])])).
fof(f2669, plain, (~ spl24_8 | ~ spl24_83), inference(avatar_split_clause, [], [f2664, f708, f393])).
fof(f393, plain, (spl24_8 <=> (e2 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_8])])).
fof(f708, plain, (spl24_83 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_83])])).
fof(f2664, plain, (~ (e2 = op(e4, e3)) | ~ spl24_83), inference(backward_demodulation, [], [f181, f710])).
fof(f710, plain, ((e2 = op(e1, e3)) | ~ spl24_83), inference(avatar_component_clause, [], [f708])).
fof(f181, plain, ~ (op(e1, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG068+1.p', ax4)).
fof(f2617, plain, (~ spl24_89 | ~ spl24_90), inference(avatar_contradiction_clause, [], [f2616])).
fof(f2616, plain, ($false | (~ spl24_89 | ~ spl24_90)), inference(subsumption_resolution, [], [f2615, f254])).
fof(f254, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f2615, plain, ((e3 = e4) | (~ spl24_89 | ~ spl24_90)), inference(forward_demodulation, [], [f739, f735])).
fof(f735, plain, ((e3 = op(e1, e2)) | ~ spl24_89), inference(avatar_component_clause, [], [f733])).
fof(f733, plain, (spl24_89 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_89])])).
fof(f739, plain, ((e4 = op(e1, e2)) | ~ spl24_90), inference(avatar_component_clause, [], [f737])).
fof(f737, plain, (spl24_90 <=> (e4 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_90])])).
fof(f2609, plain, (~ spl24_81 | ~ spl24_91), inference(avatar_split_clause, [], [f2608, f742, f700])).
fof(f700, plain, (spl24_81 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_81])])).
fof(f742, plain, (spl24_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_91])])).
fof(f2608, plain, (~ (e0 = op(e1, e3)) | ~ spl24_91), inference(forward_demodulation, [], [f210, f744])).
fof(f744, plain, ((e0 = op(e1, e1)) | ~ spl24_91), inference(avatar_component_clause, [], [f742])).
fof(f210, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2579, plain, (~ spl24_117 | ~ spl24_119), inference(avatar_contradiction_clause, [], [f2578])).
fof(f2578, plain, ($false | (~ spl24_117 | ~ spl24_119)), inference(subsumption_resolution, [], [f2577, f250])).
fof(f250, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f2577, plain, ((e1 = e3) | (~ spl24_117 | ~ spl24_119)), inference(forward_demodulation, [], [f861, f853])).
fof(f853, plain, ((e1 = op(e0, e1)) | ~ spl24_117), inference(avatar_component_clause, [], [f851])).
fof(f851, plain, (spl24_117 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_117])])).
fof(f861, plain, ((e3 = op(e0, e1)) | ~ spl24_119), inference(avatar_component_clause, [], [f859])).
fof(f859, plain, (spl24_119 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_119])])).
fof(f2561, plain, (spl24_52 | ~ spl24_78 | ~ spl24_182), inference(avatar_contradiction_clause, [], [f2560])).
fof(f2560, plain, ($false | (spl24_52 | ~ spl24_78 | ~ spl24_182)), inference(subsumption_resolution, [], [f2559, f579])).
fof(f579, plain, (~ (e1 = op(e2, e4)) | spl24_52), inference(avatar_component_clause, [], [f578])).
fof(f578, plain, (spl24_52 <=> (e1 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_52])])).
fof(f2559, plain, ((e1 = op(e2, e4)) | (~ spl24_78 | ~ spl24_182)), inference(forward_demodulation, [], [f1211, f689])).
fof(f689, plain, ((e2 = op(e1, e4)) | ~ spl24_78), inference(avatar_component_clause, [], [f687])).
fof(f687, plain, (spl24_78 <=> (e2 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_78])])).
fof(f1211, plain, ((e1 = op(op(e1, e4), e4)) | ~ spl24_182), inference(avatar_component_clause, [], [f1209])).
fof(f1209, plain, (spl24_182 <=> (e1 = op(op(e1, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_182])])).
fof(f2554, plain, (~ spl24_19 | ~ spl24_85 | spl24_184), inference(avatar_contradiction_clause, [], [f2553])).
fof(f2553, plain, ($false | (~ spl24_19 | ~ spl24_85 | spl24_184)), inference(subsumption_resolution, [], [f2552, f441])).
fof(f441, plain, ((e3 = op(e4, e1)) | ~ spl24_19), inference(avatar_component_clause, [], [f439])).
fof(f439, plain, (spl24_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_19])])).
fof(f2552, plain, (~ (e3 = op(e4, e1)) | (~ spl24_85 | spl24_184)), inference(forward_demodulation, [], [f1221, f718])).
fof(f718, plain, ((e4 = op(e1, e3)) | ~ spl24_85), inference(avatar_component_clause, [], [f716])).
fof(f716, plain, (spl24_85 <=> (e4 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_85])])).
fof(f1221, plain, (~ (e3 = op(op(e1, e3), e1)) | spl24_184), inference(avatar_component_clause, [], [f1219])).
fof(f1219, plain, (spl24_184 <=> (e3 = op(op(e1, e3), e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_184])])).
fof(f2548, plain, (~ spl24_43 | ~ spl24_57 | spl24_157), inference(avatar_contradiction_clause, [], [f2547])).
fof(f2547, plain, ($false | (~ spl24_43 | ~ spl24_57 | spl24_157)), inference(subsumption_resolution, [], [f2546, f601])).
fof(f601, plain, ((e1 = op(e2, e3)) | ~ spl24_57), inference(avatar_component_clause, [], [f599])).
fof(f599, plain, (spl24_57 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_57])])).
fof(f2546, plain, (~ (e1 = op(e2, e3)) | (~ spl24_43 | spl24_157)), inference(forward_demodulation, [], [f1089, f542])).
fof(f542, plain, ((e2 = op(e3, e1)) | ~ spl24_43), inference(avatar_component_clause, [], [f540])).
fof(f540, plain, (spl24_43 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_43])])).
fof(f1089, plain, (~ (e1 = op(op(e3, e1), e3)) | spl24_157), inference(avatar_component_clause, [], [f1087])).
fof(f1087, plain, (spl24_157 <=> (e1 = op(op(e3, e1), e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_157])])).
fof(f2535, plain, (~ spl24_27 | ~ spl24_54 | ~ spl24_166), inference(avatar_contradiction_clause, [], [f2534])).
fof(f2534, plain, ($false | (~ spl24_27 | ~ spl24_54 | ~ spl24_166)), inference(subsumption_resolution, [], [f2533, f249])).
fof(f249, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f2533, plain, ((e1 = e2) | (~ spl24_27 | ~ spl24_54 | ~ spl24_166)), inference(forward_demodulation, [], [f2532, f475])).
fof(f475, plain, ((e1 = op(e3, e4)) | ~ spl24_27), inference(avatar_component_clause, [], [f473])).
fof(f473, plain, (spl24_27 <=> (e1 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_27])])).
fof(f2532, plain, ((e2 = op(e3, e4)) | (~ spl24_54 | ~ spl24_166)), inference(forward_demodulation, [], [f1132, f588])).
fof(f588, plain, ((e3 = op(e2, e4)) | ~ spl24_54), inference(avatar_component_clause, [], [f586])).
fof(f586, plain, (spl24_54 <=> (e3 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_54])])).
fof(f1132, plain, ((e2 = op(op(e2, e4), e4)) | ~ spl24_166), inference(avatar_component_clause, [], [f1130])).
fof(f1130, plain, (spl24_166 <=> (e2 = op(op(e2, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_166])])).
fof(f2524, plain, (spl24_37 | ~ spl24_89 | ~ spl24_188), inference(avatar_contradiction_clause, [], [f2523])).
fof(f2523, plain, ($false | (spl24_37 | ~ spl24_89 | ~ spl24_188)), inference(subsumption_resolution, [], [f2522, f516])).
fof(f516, plain, (~ (e1 = op(e3, e2)) | spl24_37), inference(avatar_component_clause, [], [f515])).
fof(f515, plain, (spl24_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_37])])).
fof(f2522, plain, ((e1 = op(e3, e2)) | (~ spl24_89 | ~ spl24_188)), inference(forward_demodulation, [], [f1241, f735])).
fof(f1241, plain, ((e1 = op(op(e1, e2), e2)) | ~ spl24_188), inference(avatar_component_clause, [], [f1239])).
fof(f1239, plain, (spl24_188 <=> (e1 = op(op(e1, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_188])])).
fof(f2513, plain, (spl24_18 | ~ spl24_70 | ~ spl24_174), inference(avatar_contradiction_clause, [], [f2512])).
fof(f2512, plain, ($false | (spl24_18 | ~ spl24_70 | ~ spl24_174)), inference(subsumption_resolution, [], [f2511, f436])).
fof(f436, plain, (~ (e2 = op(e4, e1)) | spl24_18), inference(avatar_component_clause, [], [f435])).
fof(f435, plain, (spl24_18 <=> (e2 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_18])])).
fof(f2511, plain, ((e2 = op(e4, e1)) | (~ spl24_70 | ~ spl24_174)), inference(forward_demodulation, [], [f1173, f655])).
fof(f655, plain, ((e4 = op(e2, e1)) | ~ spl24_70), inference(avatar_component_clause, [], [f653])).
fof(f653, plain, (spl24_70 <=> (e4 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_70])])).
fof(f1173, plain, ((e2 = op(op(e2, e1), e1)) | ~ spl24_174), inference(avatar_component_clause, [], [f1171])).
fof(f1171, plain, (spl24_174 <=> (e2 = op(op(e2, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_174])])).
fof(f2501, plain, (~ spl24_113 | ~ spl24_115), inference(avatar_contradiction_clause, [], [f2500])).
fof(f2500, plain, ($false | (~ spl24_113 | ~ spl24_115)), inference(subsumption_resolution, [], [f2499, f253])).
fof(f2499, plain, ((e2 = e4) | (~ spl24_113 | ~ spl24_115)), inference(forward_demodulation, [], [f844, f836])).
fof(f836, plain, ((e2 = op(e0, e2)) | ~ spl24_113), inference(avatar_component_clause, [], [f834])).
fof(f834, plain, (spl24_113 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_113])])).
fof(f844, plain, ((e4 = op(e0, e2)) | ~ spl24_115), inference(avatar_component_clause, [], [f842])).
fof(f842, plain, (spl24_115 <=> (e4 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_115])])).
fof(f2497, plain, (~ spl24_39 | ~ spl24_49), inference(avatar_split_clause, [], [f2496, f565, f523])).
fof(f523, plain, (spl24_39 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_39])])).
fof(f565, plain, (spl24_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_49])])).
fof(f2496, plain, (~ (e3 = op(e3, e2)) | ~ spl24_49), inference(forward_demodulation, [], [f226, f567])).
fof(f567, plain, ((e3 = op(e3, e0)) | ~ spl24_49), inference(avatar_component_clause, [], [f565])).
fof(f226, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f2485, plain, (~ spl24_8 | ~ spl24_40 | spl24_153), inference(avatar_contradiction_clause, [], [f2484])).
fof(f2484, plain, ($false | (~ spl24_8 | ~ spl24_40 | spl24_153)), inference(subsumption_resolution, [], [f2483, f395])).
fof(f395, plain, ((e2 = op(e4, e3)) | ~ spl24_8), inference(avatar_component_clause, [], [f393])).
fof(f2483, plain, (~ (e2 = op(e4, e3)) | (~ spl24_40 | spl24_153)), inference(forward_demodulation, [], [f1070, f529])).
fof(f529, plain, ((e4 = op(e3, e2)) | ~ spl24_40), inference(avatar_component_clause, [], [f527])).
fof(f527, plain, (spl24_40 <=> (e4 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_40])])).
fof(f1070, plain, (~ (e2 = op(op(e3, e2), e3)) | spl24_153), inference(avatar_component_clause, [], [f1068])).
fof(f1068, plain, (spl24_153 <=> (e2 = op(op(e3, e2), e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_153])])).
fof(f2473, plain, (~ spl24_27 | ~ spl24_78 | ~ spl24_149), inference(avatar_contradiction_clause, [], [f2472])).
fof(f2472, plain, ($false | (~ spl24_27 | ~ spl24_78 | ~ spl24_149)), inference(subsumption_resolution, [], [f2471, f252])).
fof(f252, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f2471, plain, ((e2 = e3) | (~ spl24_27 | ~ spl24_78 | ~ spl24_149)), inference(forward_demodulation, [], [f2470, f689])).
fof(f2470, plain, ((e3 = op(e1, e4)) | (~ spl24_27 | ~ spl24_149)), inference(forward_demodulation, [], [f1049, f475])).
fof(f1049, plain, ((e3 = op(op(e3, e4), e4)) | ~ spl24_149), inference(avatar_component_clause, [], [f1047])).
fof(f1047, plain, (spl24_149 <=> (e3 = op(op(e3, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_149])])).
fof(f2462, plain, (~ spl24_25 | ~ spl24_105 | spl24_146), inference(avatar_contradiction_clause, [], [f2461])).
fof(f2461, plain, ($false | (~ spl24_25 | ~ spl24_105 | spl24_146)), inference(subsumption_resolution, [], [f2460, f802])).
fof(f802, plain, ((e4 = op(e0, e4)) | ~ spl24_105), inference(avatar_component_clause, [], [f800])).
fof(f800, plain, (spl24_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_105])])).
fof(f2460, plain, (~ (e4 = op(e0, e4)) | (~ spl24_25 | spl24_146)), inference(forward_demodulation, [], [f1035, f466])).
fof(f1035, plain, (~ (op(e0, e4) = op(e4, e0)) | spl24_146), inference(avatar_component_clause, [], [f1033])).
fof(f1033, plain, (spl24_146 <=> (op(e0, e4) = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_146])])).
fof(f2459, plain, (~ spl24_19 | ~ spl24_27 | spl24_140), inference(avatar_contradiction_clause, [], [f2458])).
fof(f2458, plain, ($false | (~ spl24_19 | ~ spl24_27 | spl24_140)), inference(subsumption_resolution, [], [f2457, f475])).
fof(f2457, plain, (~ (e1 = op(e3, e4)) | (~ spl24_19 | spl24_140)), inference(forward_demodulation, [], [f1006, f441])).
fof(f1006, plain, (~ (e1 = op(op(e4, e1), e4)) | spl24_140), inference(avatar_component_clause, [], [f1004])).
fof(f1004, plain, (spl24_140 <=> (e1 = op(op(e4, e1), e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_140])])).
fof(f2443, plain, (~ spl24_49 | ~ spl24_109 | spl24_163), inference(avatar_contradiction_clause, [], [f2442])).
fof(f2442, plain, ($false | (~ spl24_49 | ~ spl24_109 | spl24_163)), inference(subsumption_resolution, [], [f2441, f819])).
fof(f819, plain, ((e3 = op(e0, e3)) | ~ spl24_109), inference(avatar_component_clause, [], [f817])).
fof(f817, plain, (spl24_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_109])])).
fof(f2441, plain, (~ (e3 = op(e0, e3)) | (~ spl24_49 | spl24_163)), inference(forward_demodulation, [], [f1118, f567])).
fof(f1118, plain, (~ (op(e0, e3) = op(e3, e0)) | spl24_163), inference(avatar_component_clause, [], [f1116])).
fof(f1116, plain, (spl24_163 <=> (op(e0, e3) = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_163])])).
fof(f2431, plain, (~ spl24_97 | ~ spl24_99), inference(avatar_contradiction_clause, [], [f2430])).
fof(f2430, plain, ($false | (~ spl24_97 | ~ spl24_99)), inference(subsumption_resolution, [], [f2429, f250])).
fof(f2429, plain, ((e1 = e3) | (~ spl24_97 | ~ spl24_99)), inference(forward_demodulation, [], [f777, f769])).
fof(f769, plain, ((e1 = op(e1, e0)) | ~ spl24_97), inference(avatar_component_clause, [], [f767])).
fof(f767, plain, (spl24_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_97])])).
fof(f777, plain, ((e3 = op(e1, e0)) | ~ spl24_99), inference(avatar_component_clause, [], [f775])).
fof(f775, plain, (spl24_99 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_99])])).
fof(f2422, plain, (~ spl24_12 | spl24_90 | ~ spl24_137), inference(avatar_contradiction_clause, [], [f2421])).
fof(f2421, plain, ($false | (~ spl24_12 | spl24_90 | ~ spl24_137)), inference(subsumption_resolution, [], [f2420, f738])).
fof(f738, plain, (~ (e4 = op(e1, e2)) | spl24_90), inference(avatar_component_clause, [], [f737])).
fof(f2420, plain, ((e4 = op(e1, e2)) | (~ spl24_12 | ~ spl24_137)), inference(forward_demodulation, [], [f992, f412])).
fof(f412, plain, ((e1 = op(e4, e2)) | ~ spl24_12), inference(avatar_component_clause, [], [f410])).
fof(f410, plain, (spl24_12 <=> (e1 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_12])])).
fof(f992, plain, ((e4 = op(op(e4, e2), e2)) | ~ spl24_137), inference(avatar_component_clause, [], [f990])).
fof(f990, plain, (spl24_137 <=> (e4 = op(op(e4, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_137])])).
fof(f2415, plain, (~ spl24_89 | ~ spl24_57 | spl24_168), inference(avatar_split_clause, [], [f2408, f1140, f599, f733])).
fof(f1140, plain, (spl24_168 <=> (e3 = op(op(e2, e3), e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_168])])).
fof(f2408, plain, (~ (e3 = op(e1, e2)) | (~ spl24_57 | spl24_168)), inference(forward_demodulation, [], [f1142, f601])).
fof(f1142, plain, (~ (e3 = op(op(e2, e3), e2)) | spl24_168), inference(avatar_component_clause, [], [f1140])).
fof(f2402, plain, (~ spl24_73 | ~ spl24_113 | spl24_179), inference(avatar_contradiction_clause, [], [f2401])).
fof(f2401, plain, ($false | (~ spl24_73 | ~ spl24_113 | spl24_179)), inference(subsumption_resolution, [], [f2400, f836])).
fof(f2400, plain, (~ (e2 = op(e0, e2)) | (~ spl24_73 | spl24_179)), inference(forward_demodulation, [], [f1197, f668])).
fof(f668, plain, ((e2 = op(e2, e0)) | ~ spl24_73), inference(avatar_component_clause, [], [f666])).
fof(f666, plain, (spl24_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_73])])).
fof(f1197, plain, (~ (op(e0, e2) = op(e2, e0)) | spl24_179), inference(avatar_component_clause, [], [f1195])).
fof(f1195, plain, (spl24_179 <=> (op(e0, e2) = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_179])])).
fof(f2352, plain, (~ spl24_97 | ~ spl24_117 | spl24_205), inference(avatar_contradiction_clause, [], [f2351])).
fof(f2351, plain, ($false | (~ spl24_97 | ~ spl24_117 | spl24_205)), inference(subsumption_resolution, [], [f2349, f769])).
fof(f2349, plain, (~ (e1 = op(e1, e0)) | (~ spl24_117 | spl24_205)), inference(backward_demodulation, [], [f1326, f853])).
fof(f1326, plain, (~ (e1 = op(op(e0, e1), e0)) | spl24_205), inference(avatar_component_clause, [], [f1324])).
fof(f1324, plain, (spl24_205 <=> (e1 = op(op(e0, e1), e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_205])])).
fof(f2333, plain, (spl24_25 | ~ spl24_126), inference(avatar_split_clause, [], [f2324, f889, f464])).
fof(f889, plain, (spl24_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_126])])).
fof(f2324, plain, ((e4 = op(e4, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f93, f891])).
fof(f891, plain, ((e0 = unit) | ~ spl24_126), inference(avatar_component_clause, [], [f889])).
fof(f93, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG068+1.p', ax2)).
fof(f2332, plain, (spl24_105 | ~ spl24_126), inference(avatar_split_clause, [], [f2323, f889, f800])).
fof(f2323, plain, ((e4 = op(e0, e4)) | ~ spl24_126), inference(backward_demodulation, [], [f92, f891])).
fof(f92, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f2331, plain, (spl24_117 | ~ spl24_126), inference(avatar_split_clause, [], [f2317, f889, f851])).
fof(f2317, plain, ((e1 = op(e0, e1)) | ~ spl24_126), inference(backward_demodulation, [], [f86, f891])).
fof(f86, plain, (e1 = op(unit, e1)), inference(cnf_transformation, [], [f2])).
fof(f2314, plain, (~ spl24_117 | ~ spl24_97 | spl24_194), inference(avatar_split_clause, [], [f2313, f1270, f767, f851])).
fof(f1270, plain, (spl24_194 <=> (op(e0, e1) = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_194])])).
fof(f2313, plain, (~ (e1 = op(e0, e1)) | (~ spl24_97 | spl24_194)), inference(forward_demodulation, [], [f1272, f769])).
fof(f1272, plain, (~ (op(e0, e1) = op(e1, e0)) | spl24_194), inference(avatar_component_clause, [], [f1270])).
fof(f2311, plain, (~ spl24_117 | ~ spl24_67), inference(avatar_split_clause, [], [f2310, f641, f851])).
fof(f641, plain, (spl24_67 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_67])])).
fof(f2310, plain, (~ (e1 = op(e0, e1)) | ~ spl24_67), inference(forward_demodulation, [], [f156, f643])).
fof(f643, plain, ((e1 = op(e2, e1)) | ~ spl24_67), inference(avatar_component_clause, [], [f641])).
fof(f156, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f2273, plain, (~ spl24_38 | ~ spl24_40), inference(avatar_contradiction_clause, [], [f2272])).
fof(f2272, plain, ($false | (~ spl24_38 | ~ spl24_40)), inference(subsumption_resolution, [], [f2271, f253])).
fof(f2271, plain, ((e2 = e4) | (~ spl24_38 | ~ spl24_40)), inference(backward_demodulation, [], [f529, f521])).
fof(f521, plain, ((e2 = op(e3, e2)) | ~ spl24_38), inference(avatar_component_clause, [], [f519])).
fof(f519, plain, (spl24_38 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_38])])).
fof(f2254, plain, (~ spl24_73 | ~ spl24_75), inference(avatar_contradiction_clause, [], [f2253])).
fof(f2253, plain, ($false | (~ spl24_73 | ~ spl24_75)), inference(subsumption_resolution, [], [f2252, f253])).
fof(f2252, plain, ((e2 = e4) | (~ spl24_73 | ~ spl24_75)), inference(backward_demodulation, [], [f676, f668])).
fof(f676, plain, ((e4 = op(e2, e0)) | ~ spl24_75), inference(avatar_component_clause, [], [f674])).
fof(f674, plain, (spl24_75 <=> (e4 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_75])])).
fof(f2236, plain, (~ spl24_12 | ~ spl24_130), inference(avatar_contradiction_clause, [], [f2235])).
fof(f2235, plain, ($false | (~ spl24_12 | ~ spl24_130)), inference(subsumption_resolution, [], [f2234, f249])).
fof(f2234, plain, ((e1 = e2) | (~ spl24_12 | ~ spl24_130)), inference(forward_demodulation, [], [f2222, f412])).
fof(f2222, plain, ((e2 = op(e4, e2)) | ~ spl24_130), inference(backward_demodulation, [], [f88, f907])).
fof(f907, plain, ((e4 = unit) | ~ spl24_130), inference(avatar_component_clause, [], [f905])).
fof(f905, plain, (spl24_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_130])])).
fof(f88, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f2199, plain, (~ spl24_44 | ~ spl24_49), inference(avatar_split_clause, [], [f2198, f565, f544])).
fof(f544, plain, (spl24_44 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_44])])).
fof(f2198, plain, (~ (e3 = op(e3, e1)) | ~ spl24_49), inference(forward_demodulation, [], [f225, f567])).
fof(f225, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f2178, plain, (~ spl24_12 | ~ spl24_13), inference(avatar_contradiction_clause, [], [f2177])).
fof(f2177, plain, ($false | (~ spl24_12 | ~ spl24_13)), inference(subsumption_resolution, [], [f2176, f249])).
fof(f2176, plain, ((e1 = e2) | (~ spl24_12 | ~ spl24_13)), inference(forward_demodulation, [], [f416, f412])).
fof(f416, plain, ((e2 = op(e4, e2)) | ~ spl24_13), inference(avatar_component_clause, [], [f414])).
fof(f414, plain, (spl24_13 <=> (e2 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_13])])).
fof(f2160, plain, (~ spl24_48 | ~ spl24_49), inference(avatar_contradiction_clause, [], [f2159])).
fof(f2159, plain, ($false | (~ spl24_48 | ~ spl24_49)), inference(subsumption_resolution, [], [f2158, f252])).
fof(f2158, plain, ((e2 = e3) | (~ spl24_48 | ~ spl24_49)), inference(forward_demodulation, [], [f567, f563])).
fof(f563, plain, ((e2 = op(e3, e0)) | ~ spl24_48), inference(avatar_component_clause, [], [f561])).
fof(f561, plain, (spl24_48 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_48])])).
fof(f2125, plain, (spl24_82 | ~ spl24_129), inference(avatar_contradiction_clause, [], [f2124])).
fof(f2124, plain, ($false | (spl24_82 | ~ spl24_129)), inference(subsumption_resolution, [], [f2113, f705])).
fof(f705, plain, (~ (e1 = op(e1, e3)) | spl24_82), inference(avatar_component_clause, [], [f704])).
fof(f704, plain, (spl24_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_82])])).
fof(f2113, plain, ((e1 = op(e1, e3)) | ~ spl24_129), inference(backward_demodulation, [], [f87, f903])).
fof(f903, plain, ((e3 = unit) | ~ spl24_129), inference(avatar_component_clause, [], [f901])).
fof(f901, plain, (spl24_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_129])])).
fof(f87, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f2104, plain, (~ spl24_62 | ~ spl24_12), inference(avatar_split_clause, [], [f1934, f410, f620])).
fof(f620, plain, (spl24_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_62])])).
fof(f1934, plain, (~ (e1 = op(e2, e2)) | ~ spl24_12), inference(forward_demodulation, [], [f173, f412])).
fof(f173, plain, ~ (op(e2, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f2070, plain, (~ spl24_53 | ~ spl24_54), inference(avatar_contradiction_clause, [], [f2069])).
fof(f2069, plain, ($false | (~ spl24_53 | ~ spl24_54)), inference(subsumption_resolution, [], [f2068, f252])).
fof(f2068, plain, ((e2 = e3) | (~ spl24_53 | ~ spl24_54)), inference(backward_demodulation, [], [f588, f584])).
fof(f584, plain, ((e2 = op(e2, e4)) | ~ spl24_53), inference(avatar_component_clause, [], [f582])).
fof(f582, plain, (spl24_53 <=> (e2 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_53])])).
fof(f1988, plain, (~ spl24_12 | ~ spl24_128), inference(avatar_contradiction_clause, [], [f1987])).
fof(f1987, plain, ($false | (~ spl24_12 | ~ spl24_128)), inference(subsumption_resolution, [], [f1986, f251])).
fof(f251, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f1986, plain, ((e1 = e4) | (~ spl24_12 | ~ spl24_128)), inference(forward_demodulation, [], [f1970, f412])).
fof(f1970, plain, ((e4 = op(e4, e2)) | ~ spl24_128), inference(backward_demodulation, [], [f93, f899])).
fof(f899, plain, ((e2 = unit) | ~ spl24_128), inference(avatar_component_clause, [], [f897])).
fof(f897, plain, (spl24_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_128])])).
fof(f1950, plain, (spl24_91 | ~ spl24_12), inference(avatar_split_clause, [], [f1675, f410, f742])).
fof(f1675, plain, ((e0 = op(e1, e1)) | ~ spl24_12), inference(forward_demodulation, [], [f255, f412])).
fof(f255, plain, (e0 = op(op(e4, e2), op(e4, e2))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e3 = op(e2, e4)) & (e1 = op(e4, e2)) & (e0 = op(op(e4, e2), op(e4, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG068+1.p', ax6)).
fof(f1942, plain, (~ spl24_79 | ~ spl24_54), inference(avatar_split_clause, [], [f1747, f586, f691])).
fof(f691, plain, (spl24_79 <=> (e3 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_79])])).
fof(f1747, plain, (~ (e3 = op(e1, e4)) | ~ spl24_54), inference(forward_demodulation, [], [f189, f588])).
fof(f189, plain, ~ (op(e1, e4) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f1938, plain, (~ spl24_69 | ~ spl24_54), inference(avatar_split_clause, [], [f1937, f586, f649])).
fof(f649, plain, (spl24_69 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_69])])).
fof(f1937, plain, (~ (e3 = op(e2, e1)) | ~ spl24_54), inference(forward_demodulation, [], [f221, f588])).
fof(f221, plain, ~ (op(e2, e1) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f1901, plain, (~ spl24_8 | ~ spl24_54 | spl24_132), inference(avatar_contradiction_clause, [], [f1900])).
fof(f1900, plain, ($false | (~ spl24_8 | ~ spl24_54 | spl24_132)), inference(subsumption_resolution, [], [f1894, f588])).
fof(f1894, plain, (~ (e3 = op(e2, e4)) | (~ spl24_8 | spl24_132)), inference(backward_demodulation, [], [f968, f395])).
fof(f968, plain, (~ (e3 = op(op(e4, e3), e4)) | spl24_132), inference(avatar_component_clause, [], [f966])).
fof(f966, plain, (spl24_132 <=> (e3 = op(op(e4, e3), e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_132])])).
fof(f1841, plain, (~ spl24_60 | ~ spl24_85), inference(avatar_split_clause, [], [f1840, f716, f611])).
fof(f611, plain, (spl24_60 <=> (e4 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_60])])).
fof(f1840, plain, (~ (e4 = op(e2, e3)) | ~ spl24_85), inference(backward_demodulation, [], [f179, f718])).
fof(f179, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f1838, plain, (~ spl24_82 | ~ spl24_97), inference(avatar_split_clause, [], [f1835, f767, f704])).
fof(f1835, plain, (~ (e1 = op(e1, e3)) | ~ spl24_97), inference(backward_demodulation, [], [f207, f769])).
fof(f207, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1837, plain, (~ spl24_47 | ~ spl24_97), inference(avatar_split_clause, [], [f1834, f767, f557])).
fof(f557, plain, (spl24_47 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_47])])).
fof(f1834, plain, (~ (e1 = op(e3, e0)) | ~ spl24_97), inference(backward_demodulation, [], [f150, f769])).
fof(f150, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f1836, plain, (~ spl24_72 | ~ spl24_97), inference(avatar_split_clause, [], [f1833, f767, f662])).
fof(f662, plain, (spl24_72 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_72])])).
fof(f1833, plain, (~ (e1 = op(e2, e0)) | ~ spl24_97), inference(backward_demodulation, [], [f149, f769])).
fof(f149, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f1830, plain, (~ spl24_84 | ~ spl24_109), inference(avatar_split_clause, [], [f1828, f817, f712])).
fof(f712, plain, (spl24_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_84])])).
fof(f1828, plain, (~ (e3 = op(e1, e3)) | ~ spl24_109), inference(backward_demodulation, [], [f175, f819])).
fof(f175, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1821, plain, (~ spl24_42 | ~ spl24_117), inference(avatar_split_clause, [], [f1818, f851, f536])).
fof(f536, plain, (spl24_42 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_42])])).
fof(f1818, plain, (~ (e1 = op(e3, e1)) | ~ spl24_117), inference(backward_demodulation, [], [f157, f853])).
fof(f157, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f1799, plain, (spl24_96 | ~ spl24_127), inference(avatar_split_clause, [], [f1789, f893, f763])).
fof(f763, plain, (spl24_96 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_96])])).
fof(f893, plain, (spl24_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_127])])).
fof(f1789, plain, ((e0 = op(e1, e0)) | ~ spl24_127), inference(backward_demodulation, [], [f84, f895])).
fof(f895, plain, ((e1 = unit) | ~ spl24_127), inference(avatar_component_clause, [], [f893])).
fof(f84, plain, (e0 = op(unit, e0)), inference(cnf_transformation, [], [f2])).
fof(f1766, plain, (~ spl24_103 | ~ spl24_105), inference(avatar_contradiction_clause, [], [f1765])).
fof(f1765, plain, ($false | (~ spl24_103 | ~ spl24_105)), inference(subsumption_resolution, [], [f1764, f253])).
fof(f1764, plain, ((e2 = e4) | (~ spl24_103 | ~ spl24_105)), inference(forward_demodulation, [], [f802, f794])).
fof(f794, plain, ((e2 = op(e0, e4)) | ~ spl24_103), inference(avatar_component_clause, [], [f792])).
fof(f792, plain, (spl24_103 <=> (e2 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_103])])).
fof(f1712, plain, (~ spl24_37 | ~ spl24_12), inference(avatar_split_clause, [], [f1711, f410, f515])).
fof(f1711, plain, (~ (e1 = op(e3, e2)) | ~ spl24_12), inference(forward_demodulation, [], [f174, f412])).
fof(f174, plain, ~ (op(e3, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1639, plain, (~ spl24_18 | ~ spl24_19), inference(avatar_contradiction_clause, [], [f1638])).
fof(f1638, plain, ($false | (~ spl24_18 | ~ spl24_19)), inference(subsumption_resolution, [], [f1637, f252])).
fof(f1637, plain, ((e2 = e3) | (~ spl24_18 | ~ spl24_19)), inference(backward_demodulation, [], [f441, f437])).
fof(f437, plain, ((e2 = op(e4, e1)) | ~ spl24_18), inference(avatar_component_clause, [], [f435])).
fof(f1616, plain, (~ spl24_15 | ~ spl24_25), inference(avatar_split_clause, [], [f1612, f464, f422])).
fof(f422, plain, (spl24_15 <=> (e4 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_15])])).
fof(f1612, plain, (~ (e4 = op(e4, e2)) | ~ spl24_25), inference(backward_demodulation, [], [f236, f466])).
fof(f236, plain, ~ (op(e4, e0) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1607, plain, (~ spl24_27 | ~ spl24_28), inference(avatar_contradiction_clause, [], [f1606])).
fof(f1606, plain, ($false | (~ spl24_27 | ~ spl24_28)), inference(subsumption_resolution, [], [f1605, f249])).
fof(f1605, plain, ((e1 = e2) | (~ spl24_27 | ~ spl24_28)), inference(backward_demodulation, [], [f479, f475])).
fof(f479, plain, ((e2 = op(e3, e4)) | ~ spl24_28), inference(avatar_component_clause, [], [f477])).
fof(f477, plain, (spl24_28 <=> (e2 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_28])])).
fof(f1568, plain, (~ spl24_52 | ~ spl24_54), inference(avatar_contradiction_clause, [], [f1567])).
fof(f1567, plain, ($false | (~ spl24_52 | ~ spl24_54)), inference(subsumption_resolution, [], [f1566, f250])).
fof(f1566, plain, ((e1 = e3) | (~ spl24_52 | ~ spl24_54)), inference(backward_demodulation, [], [f588, f580])).
fof(f580, plain, ((e1 = op(e2, e4)) | ~ spl24_52), inference(avatar_component_clause, [], [f578])).
fof(f1565, plain, (~ spl24_54 | ~ spl24_55), inference(avatar_contradiction_clause, [], [f1564])).
fof(f1564, plain, ($false | (~ spl24_54 | ~ spl24_55)), inference(subsumption_resolution, [], [f1563, f254])).
fof(f1563, plain, ((e3 = e4) | (~ spl24_54 | ~ spl24_55)), inference(backward_demodulation, [], [f592, f588])).
fof(f592, plain, ((e4 = op(e2, e4)) | ~ spl24_55), inference(avatar_component_clause, [], [f590])).
fof(f590, plain, (spl24_55 <=> (e4 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_55])])).
fof(f1472, plain, (~ spl24_91 | ~ spl24_96), inference(avatar_split_clause, [], [f1465, f763, f742])).
fof(f1465, plain, (~ (e0 = op(e1, e1)) | ~ spl24_96), inference(backward_demodulation, [], [f205, f765])).
fof(f765, plain, ((e0 = op(e1, e0)) | ~ spl24_96), inference(avatar_component_clause, [], [f763])).
fof(f205, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f1389, plain, (spl24_49 | ~ spl24_126), inference(avatar_split_clause, [], [f1379, f889, f565])).
fof(f1379, plain, ((e3 = op(e3, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f91, f891])).
fof(f91, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f1388, plain, (spl24_109 | ~ spl24_126), inference(avatar_split_clause, [], [f1378, f889, f817])).
fof(f1378, plain, ((e3 = op(e0, e3)) | ~ spl24_126), inference(backward_demodulation, [], [f90, f891])).
fof(f90, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f1387, plain, (spl24_73 | ~ spl24_126), inference(avatar_split_clause, [], [f1377, f889, f666])).
fof(f1377, plain, ((e2 = op(e2, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f89, f891])).
fof(f89, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f1386, plain, (spl24_113 | ~ spl24_126), inference(avatar_split_clause, [], [f1376, f889, f834])).
fof(f1376, plain, ((e2 = op(e0, e2)) | ~ spl24_126), inference(backward_demodulation, [], [f88, f891])).
fof(f1367, plain, (~ spl24_91 | spl24_97), inference(avatar_split_clause, [], [f335, f767, f742])).
fof(f335, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(definition_folding, [], [f9, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
fof(f10, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP0), inference(usedef, [], [e10])).
fof(e10, plain, (sP0 <=> (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f11, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP1), inference(usedef, [], [e11])).
fof(e11, plain, (sP1 <=> (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f12, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP2), inference(usedef, [], [e12])).
fof(e12, plain, (sP2 <=> (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f13, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP3), inference(usedef, [], [e13])).
fof(e13, plain, (sP3 <=> (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f14, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP4), inference(usedef, [], [e14])).
fof(e14, plain, (sP4 <=> (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f15, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP5), inference(usedef, [], [e15])).
fof(e15, plain, (sP5 <=> (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f16, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP6), inference(usedef, [], [e16])).
fof(e16, plain, (sP6 <=> (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f17, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP7), inference(usedef, [], [e17])).
fof(e17, plain, (sP7 <=> (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f18, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP8), inference(usedef, [], [e18])).
fof(e18, plain, (sP8 <=> (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f19, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP9), inference(usedef, [], [e19])).
fof(e19, plain, (sP9 <=> (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f20, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP10), inference(usedef, [], [e20])).
fof(e20, plain, (sP10 <=> (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f21, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP11), inference(usedef, [], [e21])).
fof(e21, plain, (sP11 <=> (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f22, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP12), inference(usedef, [], [e22])).
fof(e22, plain, (sP12 <=> (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f23, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP13), inference(usedef, [], [e23])).
fof(e23, plain, (sP13 <=> (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f24, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP14), inference(usedef, [], [e24])).
fof(e24, plain, (sP14 <=> (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f25, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP15), inference(usedef, [], [e25])).
fof(e25, plain, (sP15 <=> (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f26, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP16), inference(usedef, [], [e26])).
fof(e26, plain, (sP16 <=> (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f27, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP17), inference(usedef, [], [e27])).
fof(e27, plain, (sP17 <=> (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f28, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP18), inference(usedef, [], [e28])).
fof(e28, plain, (sP18 <=> (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f29, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP19), inference(usedef, [], [e29])).
fof(e29, plain, (sP19 <=> (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f30, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP20), inference(usedef, [], [e30])).
fof(e30, plain, (sP20 <=> (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f31, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP21), inference(usedef, [], [e31])).
fof(e31, plain, (sP21 <=> (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f32, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP22), inference(usedef, [], [e32])).
fof(e32, plain, (sP22 <=> (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f33, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP23), inference(usedef, [], [e33])).
fof(e33, plain, (sP23 <=> (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f9, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG068+1.p', co1)).
fof(f1365, plain, (~ spl24_94 | spl24_82), inference(avatar_split_clause, [], [f338, f704, f754])).
fof(f754, plain, (spl24_94 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_94])])).
fof(f338, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f34])).
fof(f1360, plain, (~ spl24_65 | spl24_53), inference(avatar_split_clause, [], [f344, f582, f632])).
fof(f632, plain, (spl24_65 <=> (e4 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_65])])).
fof(f344, plain, ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))), inference(cnf_transformation, [], [f34])).
fof(f1358, plain, (~ spl24_32 | spl24_44), inference(avatar_split_clause, [], [f346, f544, f494])).
fof(f494, plain, (spl24_32 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_32])])).
fof(f346, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f34])).
fof(f1357, plain, (~ spl24_33 | spl24_39), inference(avatar_split_clause, [], [f347, f523, f498])).
fof(f498, plain, (spl24_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_33])])).
fof(f347, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f34])).
fof(f1353, plain, (~ spl24_3 | spl24_15), inference(avatar_split_clause, [], [f352, f422, f372])).
fof(f372, plain, (spl24_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_3])])).
fof(f352, plain, ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))), inference(cnf_transformation, [], [f34])).
fof(f1351, plain, (spl24_207 | spl24_204 | spl24_201 | spl24_198 | spl24_195 | spl24_191 | spl24_189 | spl24_186 | spl24_183 | spl24_180 | spl24_176 | spl24_172 | spl24_170 | spl24_167 | spl24_164 | spl24_160 | spl24_156 | spl24_152 | spl24_150 | spl24_147 | spl24_143 | spl24_139 | spl24_135 | spl24_131), inference(avatar_split_clause, [], [f358, f962, f981, f1000, f1019, f1038, f1053, f1064, f1083, f1102, f1121, f1136, f1151, f1162, f1181, f1200, f1215, f1230, f1245, f1256, f1275, f1290, f1305, f1320, f1335])).
fof(f1335, plain, (spl24_207 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl24_207])])).
fof(f1320, plain, (spl24_204 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl24_204])])).
fof(f1305, plain, (spl24_201 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl24_201])])).
fof(f1290, plain, (spl24_198 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl24_198])])).
fof(f1275, plain, (spl24_195 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl24_195])])).
fof(f1256, plain, (spl24_191 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl24_191])])).
fof(f1245, plain, (spl24_189 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl24_189])])).
fof(f1230, plain, (spl24_186 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl24_186])])).
fof(f1215, plain, (spl24_183 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl24_183])])).
fof(f1200, plain, (spl24_180 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl24_180])])).
fof(f1181, plain, (spl24_176 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl24_176])])).
fof(f1162, plain, (spl24_172 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl24_172])])).
fof(f1151, plain, (spl24_170 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl24_170])])).
fof(f1136, plain, (spl24_167 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl24_167])])).
fof(f1121, plain, (spl24_164 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl24_164])])).
fof(f1102, plain, (spl24_160 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl24_160])])).
fof(f1083, plain, (spl24_156 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl24_156])])).
fof(f1064, plain, (spl24_152 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl24_152])])).
fof(f1053, plain, (spl24_150 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl24_150])])).
fof(f1038, plain, (spl24_147 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl24_147])])).
fof(f1019, plain, (spl24_143 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl24_143])])).
fof(f1000, plain, (spl24_139 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl24_139])])).
fof(f981, plain, (spl24_135 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl24_135])])).
fof(f962, plain, (spl24_131 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl24_131])])).
fof(f358, plain, (sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(trivial_inequality_removal, [], [f355])).
fof(f355, plain, (~ (op(e4, e4) = op(e4, e4)) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f34])).
fof(f1344, plain, ~ spl24_207, inference(avatar_split_clause, [], [f359, f1335])).
fof(f359, plain, ~ sP0, inference(trivial_inequality_removal, [], [f327])).
fof(f327, plain, (~ (op(e0, e0) = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f1327, plain, (~ spl24_204 | ~ spl24_205), inference(avatar_split_clause, [], [f326, f1324, f1320])).
fof(f326, plain, (~ (e1 = op(op(e0, e1), e0)) | ~ sP1), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f1318, plain, (~ spl24_201 | ~ spl24_179), inference(avatar_split_clause, [], [f321, f1195, f1305])).
fof(f321, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP2), inference(nnf_transformation, [], [f12])).
fof(f1303, plain, (~ spl24_198 | ~ spl24_163), inference(avatar_split_clause, [], [f318, f1116, f1290])).
fof(f318, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP3), inference(nnf_transformation, [], [f13])).
fof(f1288, plain, (~ spl24_195 | ~ spl24_146), inference(avatar_split_clause, [], [f315, f1033, f1275])).
fof(f315, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP4), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP4), inference(nnf_transformation, [], [f14])).
fof(f1273, plain, (~ spl24_191 | ~ spl24_194), inference(avatar_split_clause, [], [f312, f1270, f1256])).
fof(f312, plain, (~ (op(e0, e1) = op(e1, e0)) | ~ sP5), inference(cnf_transformation, [], [f53])).
fof(f53, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP5), inference(nnf_transformation, [], [f15])).
fof(f1254, plain, ~ spl24_189, inference(avatar_split_clause, [], [f360, f1245])).
fof(f360, plain, ~ sP6, inference(trivial_inequality_removal, [], [f309])).
fof(f309, plain, (~ (op(e1, e1) = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f1242, plain, (~ spl24_186 | spl24_188), inference(avatar_split_clause, [], [f307, f1239, f1230])).
fof(f307, plain, ((e1 = op(op(e1, e2), e2)) | ~ sP7), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP7), inference(nnf_transformation, [], [f17])).
fof(f1222, plain, (~ spl24_183 | ~ spl24_184), inference(avatar_split_clause, [], [f305, f1219, f1215])).
fof(f305, plain, (~ (e3 = op(op(e1, e3), e1)) | ~ sP8), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP8), inference(nnf_transformation, [], [f18])).
fof(f1212, plain, (~ spl24_180 | spl24_182), inference(avatar_split_clause, [], [f301, f1209, f1200])).
fof(f301, plain, ((e1 = op(op(e1, e4), e4)) | ~ sP9), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP9), inference(nnf_transformation, [], [f19])).
fof(f1198, plain, (~ spl24_176 | ~ spl24_179), inference(avatar_split_clause, [], [f297, f1195, f1181])).
fof(f297, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP10), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP10), inference(nnf_transformation, [], [f20])).
fof(f1174, plain, (~ spl24_172 | spl24_174), inference(avatar_split_clause, [], [f295, f1171, f1162])).
fof(f295, plain, ((e2 = op(op(e2, e1), e1)) | ~ sP11), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP11), inference(nnf_transformation, [], [f21])).
fof(f1160, plain, ~ spl24_170, inference(avatar_split_clause, [], [f361, f1151])).
fof(f361, plain, ~ sP12, inference(trivial_inequality_removal, [], [f291])).
fof(f291, plain, (~ (op(e2, e2) = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP12), inference(nnf_transformation, [], [f22])).
fof(f1143, plain, (~ spl24_167 | ~ spl24_168), inference(avatar_split_clause, [], [f290, f1140, f1136])).
fof(f290, plain, (~ (e3 = op(op(e2, e3), e2)) | ~ sP13), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP13), inference(nnf_transformation, [], [f23])).
fof(f1133, plain, (~ spl24_164 | spl24_166), inference(avatar_split_clause, [], [f286, f1130, f1121])).
fof(f286, plain, ((e2 = op(op(e2, e4), e4)) | ~ sP14), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP14), inference(nnf_transformation, [], [f24])).
fof(f1119, plain, (~ spl24_160 | ~ spl24_163), inference(avatar_split_clause, [], [f282, f1116, f1102])).
fof(f282, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP15), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP15), inference(nnf_transformation, [], [f25])).
fof(f1090, plain, (~ spl24_156 | ~ spl24_157), inference(avatar_split_clause, [], [f281, f1087, f1083])).
fof(f281, plain, (~ (e1 = op(op(e3, e1), e3)) | ~ sP16), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP16), inference(nnf_transformation, [], [f26])).
fof(f1071, plain, (~ spl24_152 | ~ spl24_153), inference(avatar_split_clause, [], [f278, f1068, f1064])).
fof(f278, plain, (~ (e2 = op(op(e3, e2), e3)) | ~ sP17), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP17), inference(nnf_transformation, [], [f27])).
fof(f1062, plain, ~ spl24_150, inference(avatar_split_clause, [], [f362, f1053])).
fof(f362, plain, ~ sP18, inference(trivial_inequality_removal, [], [f273])).
fof(f273, plain, (~ (op(e3, e3) = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP18), inference(nnf_transformation, [], [f28])).
fof(f1050, plain, (~ spl24_147 | spl24_149), inference(avatar_split_clause, [], [f271, f1047, f1038])).
fof(f271, plain, ((e3 = op(op(e3, e4), e4)) | ~ sP19), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP19), inference(nnf_transformation, [], [f29])).
fof(f1036, plain, (~ spl24_143 | ~ spl24_146), inference(avatar_split_clause, [], [f267, f1033, f1019])).
fof(f267, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP20), inference(cnf_transformation, [], [f38])).
fof(f38, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP20), inference(nnf_transformation, [], [f30])).
fof(f1007, plain, (~ spl24_139 | ~ spl24_140), inference(avatar_split_clause, [], [f266, f1004, f1000])).
fof(f266, plain, (~ (e1 = op(op(e4, e1), e4)) | ~ sP21), inference(cnf_transformation, [], [f37])).
fof(f37, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP21), inference(nnf_transformation, [], [f31])).
fof(f993, plain, (~ spl24_135 | spl24_137), inference(avatar_split_clause, [], [f262, f990, f981])).
fof(f262, plain, ((e4 = op(op(e4, e2), e2)) | ~ sP22), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP22), inference(nnf_transformation, [], [f32])).
fof(f969, plain, (~ spl24_131 | ~ spl24_132), inference(avatar_split_clause, [], [f260, f966, f962])).
fof(f260, plain, (~ (e3 = op(op(e4, e3), e4)) | ~ sP23), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP23), inference(nnf_transformation, [], [f33])).
fof(f960, plain, spl24_12, inference(avatar_split_clause, [], [f256, f410])).
fof(f256, plain, (e1 = op(e4, e2)), inference(cnf_transformation, [], [f6])).
fof(f959, plain, spl24_54, inference(avatar_split_clause, [], [f257, f586])).
fof(f257, plain, (e3 = op(e2, e4)), inference(cnf_transformation, [], [f6])).
fof(f942, plain, (spl24_99 | spl24_94 | spl24_89 | spl24_84 | spl24_79), inference(avatar_split_clause, [], [f111, f691, f712, f733, f754, f775])).
fof(f111, plain, ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG068+1.p', ax3)).
fof(f941, plain, (spl24_119 | spl24_94 | spl24_69 | spl24_44 | spl24_19), inference(avatar_split_clause, [], [f112, f439, f544, f649, f754, f859])).
fof(f112, plain, ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f3])).
fof(f936, plain, (spl24_72 | spl24_67 | spl24_62 | spl24_57 | spl24_52), inference(avatar_split_clause, [], [f117, f578, f599, f620, f641, f662])).
fof(f117, plain, ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f930, plain, (spl24_75 | spl24_70 | spl24_65 | spl24_60 | spl24_55), inference(avatar_split_clause, [], [f123, f590, f611, f632, f653, f674])).
fof(f123, plain, ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f929, plain, (spl24_115 | spl24_90 | spl24_65 | spl24_40 | spl24_15), inference(avatar_split_clause, [], [f124, f422, f527, f632, f737, f842])).
fof(f124, plain, ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))), inference(cnf_transformation, [], [f3])).
fof(f926, plain, (spl24_47 | spl24_42 | spl24_37 | spl24_32 | spl24_27), inference(avatar_split_clause, [], [f127, f473, f494, f515, f536, f557])).
fof(f127, plain, ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f924, plain, (spl24_48 | spl24_43 | spl24_38 | spl24_33 | spl24_28), inference(avatar_split_clause, [], [f129, f477, f498, f519, f540, f561])).
fof(f129, plain, ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f914, plain, (spl24_23 | spl24_18 | spl24_13 | spl24_8 | spl24_3), inference(avatar_split_clause, [], [f139, f372, f393, f414, f435, f456])).
fof(f139, plain, ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f913, plain, (spl24_103 | spl24_78 | spl24_53 | spl24_28 | spl24_3), inference(avatar_split_clause, [], [f140, f372, f477, f582, f687, f792])).
fof(f140, plain, ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f908, plain, (spl24_126 | spl24_127 | spl24_128 | spl24_129 | spl24_130), inference(avatar_split_clause, [], [f94, f905, f901, f897, f893, f889])).
fof(f94, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f719, plain, (spl24_81 | spl24_82 | spl24_83 | spl24_84 | spl24_85), inference(avatar_split_clause, [], [f67, f716, f712, f708, f704, f700])).
fof(f67, plain, ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG068+1.p', ax1)).