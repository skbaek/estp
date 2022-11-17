fof(f2481, plain, $false, inference(avatar_sat_refutation, [], [f406, f511, f595, f679, f700, f889, f893, f898, f899, f905, f917, f928, f940, f942, f944, f963, f977, f996, f1020, f1035, f1046, f1065, f1084, f1103, f1118, f1133, f1144, f1158, f1182, f1197, f1212, f1227, f1238, f1257, f1272, f1287, f1302, f1317, f1328, f1335, f1339, f1340, f1353, f1355, f1356, f1373, f1376, f1410, f1455, f1456, f1457, f1463, f1464, f1465, f1466, f1467, f1481, f1508, f1514, f1515, f1516, f1583, f1588, f1636, f1662, f1674, f1744, f1749, f1756, f1758, f1764, f1769, f1770, f1829, f1842, f1855, f1886, f1898, f1925, f1958, f1966, f1981, f1992, f2033, f2062, f2071, f2078, f2113, f2151, f2187, f2265, f2304, f2328, f2332, f2354, f2355, f2356, f2357, f2358, f2359, f2370, f2384, f2411, f2415, f2423, f2438, f2443, f2452, f2460, f2464, f2469, f2477])).
fof(f2477, plain, (~ spl24_97 | ~ spl24_117 | spl24_194), inference(avatar_contradiction_clause, [], [f2476])).
fof(f2476, plain, ($false | (~ spl24_97 | ~ spl24_117 | spl24_194)), inference(subsumption_resolution, [], [f2475, f834])).
fof(f834, plain, ((e1 = op(e0, e1)) | ~ spl24_117), inference(avatar_component_clause, [], [f832])).
fof(f832, plain, (spl24_117 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_117])])).
fof(f2475, plain, (~ (e1 = op(e0, e1)) | (~ spl24_97 | spl24_194)), inference(forward_demodulation, [], [f1256, f750])).
fof(f750, plain, ((e1 = op(e1, e0)) | ~ spl24_97), inference(avatar_component_clause, [], [f748])).
fof(f748, plain, (spl24_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_97])])).
fof(f1256, plain, (~ (op(e0, e1) = op(e1, e0)) | spl24_194), inference(avatar_component_clause, [], [f1254])).
fof(f1254, plain, (spl24_194 <=> (op(e0, e1) = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_194])])).
fof(f2469, plain, (~ spl24_73 | ~ spl24_113 | spl24_179), inference(avatar_contradiction_clause, [], [f2468])).
fof(f2468, plain, ($false | (~ spl24_73 | ~ spl24_113 | spl24_179)), inference(subsumption_resolution, [], [f2467, f817])).
fof(f817, plain, ((e2 = op(e0, e2)) | ~ spl24_113), inference(avatar_component_clause, [], [f815])).
fof(f815, plain, (spl24_113 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_113])])).
fof(f2467, plain, (~ (e2 = op(e0, e2)) | (~ spl24_73 | spl24_179)), inference(forward_demodulation, [], [f1181, f649])).
fof(f649, plain, ((e2 = op(e2, e0)) | ~ spl24_73), inference(avatar_component_clause, [], [f647])).
fof(f647, plain, (spl24_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_73])])).
fof(f1181, plain, (~ (op(e0, e2) = op(e2, e0)) | spl24_179), inference(avatar_component_clause, [], [f1179])).
fof(f1179, plain, (spl24_179 <=> (op(e0, e2) = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_179])])).
fof(f2464, plain, (~ spl24_66 | ~ spl24_117 | ~ spl24_174), inference(avatar_contradiction_clause, [], [f2463])).
fof(f2463, plain, ($false | (~ spl24_66 | ~ spl24_117 | ~ spl24_174)), inference(subsumption_resolution, [], [f2462, f249])).
fof(f249, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG064+1.p', ax5)).
fof(f2462, plain, ((e1 = e2) | (~ spl24_66 | ~ spl24_117 | ~ spl24_174)), inference(forward_demodulation, [], [f2461, f834])).
fof(f2461, plain, ((e2 = op(e0, e1)) | (~ spl24_66 | ~ spl24_174)), inference(forward_demodulation, [], [f1157, f620])).
fof(f620, plain, ((e0 = op(e2, e1)) | ~ spl24_66), inference(avatar_component_clause, [], [f618])).
fof(f618, plain, (spl24_66 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_66])])).
fof(f1157, plain, ((e2 = op(op(e2, e1), e1)) | ~ spl24_174), inference(avatar_component_clause, [], [f1155])).
fof(f1155, plain, (spl24_174 <=> (e2 = op(op(e2, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_174])])).
fof(f2460, plain, (~ spl24_66 | ~ spl24_86 | spl24_175), inference(avatar_contradiction_clause, [], [f2459])).
fof(f2459, plain, ($false | (~ spl24_66 | ~ spl24_86 | spl24_175)), inference(subsumption_resolution, [], [f2458, f704])).
fof(f704, plain, ((e0 = op(e1, e2)) | ~ spl24_86), inference(avatar_component_clause, [], [f702])).
fof(f702, plain, (spl24_86 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_86])])).
fof(f2458, plain, (~ (e0 = op(e1, e2)) | (~ spl24_66 | spl24_175)), inference(forward_demodulation, [], [f1162, f620])).
fof(f1162, plain, (~ (op(e1, e2) = op(e2, e1)) | spl24_175), inference(avatar_component_clause, [], [f1160])).
fof(f1160, plain, (spl24_175 <=> (op(e1, e2) = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_175])])).
fof(f2452, plain, (~ spl24_49 | ~ spl24_109 | spl24_163), inference(avatar_contradiction_clause, [], [f2451])).
fof(f2451, plain, ($false | (~ spl24_49 | ~ spl24_109 | spl24_163)), inference(subsumption_resolution, [], [f2450, f800])).
fof(f800, plain, ((e3 = op(e0, e3)) | ~ spl24_109), inference(avatar_component_clause, [], [f798])).
fof(f798, plain, (spl24_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_109])])).
fof(f2450, plain, (~ (e3 = op(e0, e3)) | (~ spl24_49 | spl24_163)), inference(forward_demodulation, [], [f1102, f548])).
fof(f548, plain, ((e3 = op(e3, e0)) | ~ spl24_49), inference(avatar_component_clause, [], [f546])).
fof(f546, plain, (spl24_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_49])])).
fof(f1102, plain, (~ (op(e0, e3) = op(e3, e0)) | spl24_163), inference(avatar_component_clause, [], [f1100])).
fof(f1100, plain, (spl24_163 <=> (op(e0, e3) = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_163])])).
fof(f2443, plain, (~ spl24_43 | ~ spl24_83 | spl24_159), inference(avatar_contradiction_clause, [], [f2442])).
fof(f2442, plain, ($false | (~ spl24_43 | ~ spl24_83 | spl24_159)), inference(subsumption_resolution, [], [f2441, f691])).
fof(f691, plain, ((e2 = op(e1, e3)) | ~ spl24_83), inference(avatar_component_clause, [], [f689])).
fof(f689, plain, (spl24_83 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_83])])).
fof(f2441, plain, (~ (e2 = op(e1, e3)) | (~ spl24_43 | spl24_159)), inference(forward_demodulation, [], [f1083, f523])).
fof(f523, plain, ((e2 = op(e3, e1)) | ~ spl24_43), inference(avatar_component_clause, [], [f521])).
fof(f521, plain, (spl24_43 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_43])])).
fof(f1083, plain, (~ (op(e1, e3) = op(e3, e1)) | spl24_159), inference(avatar_component_clause, [], [f1081])).
fof(f1081, plain, (spl24_159 <=> (op(e1, e3) = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_159])])).
fof(f2438, plain, (~ spl24_25 | ~ spl24_105 | spl24_146), inference(avatar_contradiction_clause, [], [f2437])).
fof(f2437, plain, ($false | (~ spl24_25 | ~ spl24_105 | spl24_146)), inference(subsumption_resolution, [], [f2436, f783])).
fof(f783, plain, ((e4 = op(e0, e4)) | ~ spl24_105), inference(avatar_component_clause, [], [f781])).
fof(f781, plain, (spl24_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_105])])).
fof(f2436, plain, (~ (e4 = op(e0, e4)) | (~ spl24_25 | spl24_146)), inference(forward_demodulation, [], [f1019, f447])).
fof(f447, plain, ((e4 = op(e4, e0)) | ~ spl24_25), inference(avatar_component_clause, [], [f445])).
fof(f445, plain, (spl24_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_25])])).
fof(f1019, plain, (~ (op(e0, e4) = op(e4, e0)) | spl24_146), inference(avatar_component_clause, [], [f1017])).
fof(f1017, plain, (spl24_146 <=> (op(e0, e4) = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_146])])).
fof(f2423, plain, (~ spl24_40 | ~ spl24_60 | spl24_155), inference(avatar_contradiction_clause, [], [f2422])).
fof(f2422, plain, ($false | (~ spl24_40 | ~ spl24_60 | spl24_155)), inference(subsumption_resolution, [], [f2421, f594])).
fof(f594, plain, ((e4 = op(e2, e3)) | ~ spl24_60), inference(avatar_component_clause, [], [f592])).
fof(f592, plain, (spl24_60 <=> (e4 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_60])])).
fof(f2421, plain, (~ (e4 = op(e2, e3)) | (~ spl24_40 | spl24_155)), inference(forward_demodulation, [], [f1064, f510])).
fof(f510, plain, ((e4 = op(e3, e2)) | ~ spl24_40), inference(avatar_component_clause, [], [f508])).
fof(f508, plain, (spl24_40 <=> (e4 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_40])])).
fof(f1064, plain, (~ (op(e2, e3) = op(e3, e2)) | spl24_155), inference(avatar_component_clause, [], [f1062])).
fof(f1062, plain, (spl24_155 <=> (op(e2, e3) = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_155])])).
fof(f2415, plain, (~ spl24_19 | ~ spl24_43 | ~ spl24_141), inference(avatar_contradiction_clause, [], [f2414])).
fof(f2414, plain, ($false | (~ spl24_19 | ~ spl24_43 | ~ spl24_141)), inference(subsumption_resolution, [], [f2413, f253])).
fof(f253, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f2413, plain, ((e2 = e4) | (~ spl24_19 | ~ spl24_43 | ~ spl24_141)), inference(forward_demodulation, [], [f2412, f523])).
fof(f2412, plain, ((e4 = op(e3, e1)) | (~ spl24_19 | ~ spl24_141)), inference(forward_demodulation, [], [f995, f422])).
fof(f422, plain, ((e3 = op(e4, e1)) | ~ spl24_19), inference(avatar_component_clause, [], [f420])).
fof(f420, plain, (spl24_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_19])])).
fof(f995, plain, ((e4 = op(op(e4, e1), e1)) | ~ spl24_141), inference(avatar_component_clause, [], [f993])).
fof(f993, plain, (spl24_141 <=> (e4 = op(op(e4, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_141])])).
fof(f2411, plain, (~ spl24_19 | ~ spl24_79 | spl24_142), inference(avatar_contradiction_clause, [], [f2410])).
fof(f2410, plain, ($false | (~ spl24_19 | ~ spl24_79 | spl24_142)), inference(subsumption_resolution, [], [f2409, f674])).
fof(f674, plain, ((e3 = op(e1, e4)) | ~ spl24_79), inference(avatar_component_clause, [], [f672])).
fof(f672, plain, (spl24_79 <=> (e3 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_79])])).
fof(f2409, plain, (~ (e3 = op(e1, e4)) | (~ spl24_19 | spl24_142)), inference(forward_demodulation, [], [f1000, f422])).
fof(f1000, plain, (~ (op(e1, e4) = op(e4, e1)) | spl24_142), inference(avatar_component_clause, [], [f998])).
fof(f998, plain, (spl24_142 <=> (op(e1, e4) = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_142])])).
fof(f2384, plain, (~ spl24_77 | ~ spl24_97), inference(avatar_split_clause, [], [f2379, f748, f664])).
fof(f664, plain, (spl24_77 <=> (e1 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_77])])).
fof(f2379, plain, (~ (e1 = op(e1, e4)) | ~ spl24_97), inference(backward_demodulation, [], [f208, f750])).
fof(f208, plain, ~ (op(e1, e0) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG064+1.p', ax4)).
fof(f2370, plain, (~ spl24_106 | ~ spl24_121), inference(avatar_split_clause, [], [f2363, f849, f786])).
fof(f786, plain, (spl24_106 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_106])])).
fof(f849, plain, (spl24_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_121])])).
fof(f2363, plain, (~ (e0 = op(e0, e3)) | ~ spl24_121), inference(backward_demodulation, [], [f197, f851])).
fof(f851, plain, ((e0 = op(e0, e0)) | ~ spl24_121), inference(avatar_component_clause, [], [f849])).
fof(f197, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f2359, plain, (spl24_25 | ~ spl24_126), inference(avatar_split_clause, [], [f2348, f870, f445])).
fof(f870, plain, (spl24_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_126])])).
fof(f2348, plain, ((e4 = op(e4, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f93, f872])).
fof(f872, plain, ((e0 = unit) | ~ spl24_126), inference(avatar_component_clause, [], [f870])).
fof(f93, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG064+1.p', ax2)).
fof(f2358, plain, (spl24_105 | ~ spl24_126), inference(avatar_split_clause, [], [f2347, f870, f781])).
fof(f2347, plain, ((e4 = op(e0, e4)) | ~ spl24_126), inference(backward_demodulation, [], [f92, f872])).
fof(f92, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f2357, plain, (spl24_49 | ~ spl24_126), inference(avatar_split_clause, [], [f2346, f870, f546])).
fof(f2346, plain, ((e3 = op(e3, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f91, f872])).
fof(f91, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f2356, plain, (spl24_109 | ~ spl24_126), inference(avatar_split_clause, [], [f2345, f870, f798])).
fof(f2345, plain, ((e3 = op(e0, e3)) | ~ spl24_126), inference(backward_demodulation, [], [f90, f872])).
fof(f90, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f2355, plain, (spl24_97 | ~ spl24_126), inference(avatar_split_clause, [], [f2342, f870, f748])).
fof(f2342, plain, ((e1 = op(e1, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f87, f872])).
fof(f87, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f2354, plain, (spl24_121 | ~ spl24_126), inference(avatar_split_clause, [], [f2340, f870, f849])).
fof(f2340, plain, ((e0 = op(e0, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f85, f872])).
fof(f85, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f2332, plain, (~ spl24_12 | ~ spl24_86 | ~ spl24_137), inference(avatar_contradiction_clause, [], [f2331])).
fof(f2331, plain, ($false | (~ spl24_12 | ~ spl24_86 | ~ spl24_137)), inference(subsumption_resolution, [], [f2330, f248])).
fof(f248, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f2330, plain, ((e0 = e4) | (~ spl24_12 | ~ spl24_86 | ~ spl24_137)), inference(forward_demodulation, [], [f2329, f704])).
fof(f2329, plain, ((e4 = op(e1, e2)) | (~ spl24_12 | ~ spl24_137)), inference(forward_demodulation, [], [f976, f393])).
fof(f393, plain, ((e1 = op(e4, e2)) | ~ spl24_12), inference(avatar_component_clause, [], [f391])).
fof(f391, plain, (spl24_12 <=> (e1 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_12])])).
fof(f976, plain, ((e4 = op(op(e4, e2), e2)) | ~ spl24_137), inference(avatar_component_clause, [], [f974])).
fof(f974, plain, (spl24_137 <=> (e4 = op(op(e4, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_137])])).
fof(f2328, plain, (~ spl24_12 | ~ spl24_52 | spl24_138), inference(avatar_contradiction_clause, [], [f2327])).
fof(f2327, plain, ($false | (~ spl24_12 | ~ spl24_52 | spl24_138)), inference(subsumption_resolution, [], [f2326, f561])).
fof(f561, plain, ((e1 = op(e2, e4)) | ~ spl24_52), inference(avatar_component_clause, [], [f559])).
fof(f559, plain, (spl24_52 <=> (e1 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_52])])).
fof(f2326, plain, (~ (e1 = op(e2, e4)) | (~ spl24_12 | spl24_138)), inference(forward_demodulation, [], [f981, f393])).
fof(f981, plain, (~ (op(e2, e4) = op(e4, e2)) | spl24_138), inference(avatar_component_clause, [], [f979])).
fof(f979, plain, (spl24_138 <=> (op(e2, e4) = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_138])])).
fof(f2304, plain, (~ spl24_1 | ~ spl24_3), inference(avatar_contradiction_clause, [], [f2303])).
fof(f2303, plain, ($false | (~ spl24_1 | ~ spl24_3)), inference(subsumption_resolution, [], [f2302, f246])).
fof(f246, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f2302, plain, ((e0 = e2) | (~ spl24_1 | ~ spl24_3)), inference(backward_demodulation, [], [f355, f347])).
fof(f347, plain, ((e0 = op(e4, e4)) | ~ spl24_1), inference(avatar_component_clause, [], [f345])).
fof(f345, plain, (spl24_1 <=> (e0 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_1])])).
fof(f355, plain, ((e2 = op(e4, e4)) | ~ spl24_3), inference(avatar_component_clause, [], [f353])).
fof(f353, plain, (spl24_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_3])])).
fof(f2265, plain, (spl24_106 | ~ spl24_129), inference(avatar_split_clause, [], [f2254, f882, f786])).
fof(f882, plain, (spl24_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_129])])).
fof(f2254, plain, ((e0 = op(e0, e3)) | ~ spl24_129), inference(backward_demodulation, [], [f85, f884])).
fof(f884, plain, ((e3 = unit) | ~ spl24_129), inference(avatar_component_clause, [], [f882])).
fof(f2187, plain, (~ spl24_62 | ~ spl24_64), inference(avatar_contradiction_clause, [], [f2186])).
fof(f2186, plain, ($false | (~ spl24_62 | ~ spl24_64)), inference(subsumption_resolution, [], [f2185, f250])).
fof(f250, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f2185, plain, ((e1 = e3) | (~ spl24_62 | ~ spl24_64)), inference(backward_demodulation, [], [f611, f603])).
fof(f603, plain, ((e1 = op(e2, e2)) | ~ spl24_62), inference(avatar_component_clause, [], [f601])).
fof(f601, plain, (spl24_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_62])])).
fof(f611, plain, ((e3 = op(e2, e2)) | ~ spl24_64), inference(avatar_component_clause, [], [f609])).
fof(f609, plain, (spl24_64 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_64])])).
fof(f2151, plain, (~ spl24_86 | ~ spl24_127), inference(avatar_contradiction_clause, [], [f2150])).
fof(f2150, plain, ($false | (~ spl24_86 | ~ spl24_127)), inference(subsumption_resolution, [], [f2149, f246])).
fof(f2149, plain, ((e0 = e2) | (~ spl24_86 | ~ spl24_127)), inference(forward_demodulation, [], [f2135, f704])).
fof(f2135, plain, ((e2 = op(e1, e2)) | ~ spl24_127), inference(backward_demodulation, [], [f88, f876])).
fof(f876, plain, ((e1 = unit) | ~ spl24_127), inference(avatar_component_clause, [], [f874])).
fof(f874, plain, (spl24_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_127])])).
fof(f88, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f2113, plain, (~ spl24_78 | ~ spl24_3), inference(avatar_split_clause, [], [f2112, f353, f668])).
fof(f668, plain, (spl24_78 <=> (e2 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_78])])).
fof(f2112, plain, (~ (e2 = op(e1, e4)) | ~ spl24_3), inference(forward_demodulation, [], [f191, f355])).
fof(f191, plain, ~ (op(e1, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2078, plain, (~ spl24_32 | ~ spl24_37), inference(avatar_split_clause, [], [f2074, f496, f475])).
fof(f475, plain, (spl24_32 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_32])])).
fof(f496, plain, (spl24_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_37])])).
fof(f2074, plain, (~ (e1 = op(e3, e3)) | ~ spl24_37), inference(backward_demodulation, [], [f232, f498])).
fof(f498, plain, ((e1 = op(e3, e2)) | ~ spl24_37), inference(avatar_component_clause, [], [f496])).
fof(f232, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f2071, plain, (~ spl24_32 | ~ spl24_57), inference(avatar_split_clause, [], [f2067, f580, f475])).
fof(f580, plain, (spl24_57 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_57])])).
fof(f2067, plain, (~ (e1 = op(e3, e3)) | ~ spl24_57), inference(backward_demodulation, [], [f182, f582])).
fof(f582, plain, ((e1 = op(e2, e3)) | ~ spl24_57), inference(avatar_component_clause, [], [f580])).
fof(f182, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f2062, plain, (~ spl24_92 | ~ spl24_95), inference(avatar_contradiction_clause, [], [f2061])).
fof(f2061, plain, ($false | (~ spl24_92 | ~ spl24_95)), inference(subsumption_resolution, [], [f2060, f251])).
fof(f251, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f2060, plain, ((e1 = e4) | (~ spl24_92 | ~ spl24_95)), inference(backward_demodulation, [], [f741, f729])).
fof(f729, plain, ((e1 = op(e1, e1)) | ~ spl24_92), inference(avatar_component_clause, [], [f727])).
fof(f727, plain, (spl24_92 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_92])])).
fof(f741, plain, ((e4 = op(e1, e1)) | ~ spl24_95), inference(avatar_component_clause, [], [f739])).
fof(f739, plain, (spl24_95 <=> (e4 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_95])])).
fof(f2033, plain, (~ spl24_86 | ~ spl24_128), inference(avatar_contradiction_clause, [], [f2032])).
fof(f2032, plain, ($false | (~ spl24_86 | ~ spl24_128)), inference(subsumption_resolution, [], [f2031, f245])).
fof(f245, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f2031, plain, ((e0 = e1) | (~ spl24_86 | ~ spl24_128)), inference(forward_demodulation, [], [f2016, f704])).
fof(f2016, plain, ((e1 = op(e1, e2)) | ~ spl24_128), inference(backward_demodulation, [], [f87, f880])).
fof(f880, plain, ((e2 = unit) | ~ spl24_128), inference(avatar_component_clause, [], [f878])).
fof(f878, plain, (spl24_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_128])])).
fof(f1992, plain, (~ spl24_9 | ~ spl24_109), inference(avatar_split_clause, [], [f1810, f798, f378])).
fof(f378, plain, (spl24_9 <=> (e3 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_9])])).
fof(f1810, plain, (~ (e3 = op(e4, e3)) | ~ spl24_109), inference(forward_demodulation, [], [f178, f800])).
fof(f178, plain, ~ (op(e0, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f1981, plain, (~ spl24_21 | ~ spl24_25), inference(avatar_contradiction_clause, [], [f1980])).
fof(f1980, plain, ($false | (~ spl24_21 | ~ spl24_25)), inference(subsumption_resolution, [], [f1977, f248])).
fof(f1977, plain, ((e0 = e4) | (~ spl24_21 | ~ spl24_25)), inference(backward_demodulation, [], [f447, f431])).
fof(f431, plain, ((e0 = op(e4, e0)) | ~ spl24_21), inference(avatar_component_clause, [], [f429])).
fof(f429, plain, (spl24_21 <=> (e0 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_21])])).
fof(f1966, plain, (~ spl24_91 | ~ spl24_95), inference(avatar_contradiction_clause, [], [f1965])).
fof(f1965, plain, ($false | (~ spl24_91 | ~ spl24_95)), inference(subsumption_resolution, [], [f1964, f248])).
fof(f1964, plain, ((e0 = e4) | (~ spl24_91 | ~ spl24_95)), inference(backward_demodulation, [], [f741, f725])).
fof(f725, plain, ((e0 = op(e1, e1)) | ~ spl24_91), inference(avatar_component_clause, [], [f723])).
fof(f723, plain, (spl24_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_91])])).
fof(f1958, plain, (~ spl24_3 | ~ spl24_130), inference(avatar_contradiction_clause, [], [f1957])).
fof(f1957, plain, ($false | (~ spl24_3 | ~ spl24_130)), inference(subsumption_resolution, [], [f1956, f253])).
fof(f1956, plain, ((e2 = e4) | (~ spl24_3 | ~ spl24_130)), inference(forward_demodulation, [], [f1935, f355])).
fof(f1935, plain, ((e4 = op(e4, e4)) | ~ spl24_130), inference(backward_demodulation, [], [f93, f888])).
fof(f888, plain, ((e4 = unit) | ~ spl24_130), inference(avatar_component_clause, [], [f886])).
fof(f886, plain, (spl24_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_130])])).
fof(f1925, plain, (~ spl24_122 | ~ spl24_97), inference(avatar_split_clause, [], [f1924, f748, f853])).
fof(f853, plain, (spl24_122 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl24_122])])).
fof(f1924, plain, (~ (op(e0, e0) = e1) | ~ spl24_97), inference(forward_demodulation, [], [f145, f750])).
fof(f145, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f4])).
fof(f1898, plain, (~ spl24_16 | ~ spl24_19), inference(avatar_contradiction_clause, [], [f1897])).
fof(f1897, plain, ($false | (~ spl24_16 | ~ spl24_19)), inference(subsumption_resolution, [], [f1895, f247])).
fof(f247, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f5])).
fof(f1895, plain, ((e0 = e3) | (~ spl24_16 | ~ spl24_19)), inference(backward_demodulation, [], [f422, f410])).
fof(f410, plain, ((e0 = op(e4, e1)) | ~ spl24_16), inference(avatar_component_clause, [], [f408])).
fof(f408, plain, (spl24_16 <=> (e0 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_16])])).
fof(f1886, plain, (~ spl24_51 | ~ spl24_52), inference(avatar_contradiction_clause, [], [f1885])).
fof(f1885, plain, ($false | (~ spl24_51 | ~ spl24_52)), inference(subsumption_resolution, [], [f1884, f245])).
fof(f1884, plain, ((e0 = e1) | (~ spl24_51 | ~ spl24_52)), inference(backward_demodulation, [], [f561, f557])).
fof(f557, plain, ((e0 = op(e2, e4)) | ~ spl24_51), inference(avatar_component_clause, [], [f555])).
fof(f555, plain, (spl24_51 <=> (e0 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_51])])).
fof(f1855, plain, (~ spl24_67 | ~ spl24_117), inference(avatar_split_clause, [], [f1854, f832, f622])).
fof(f622, plain, (spl24_67 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_67])])).
fof(f1854, plain, (~ (e1 = op(e2, e1)) | ~ spl24_117), inference(forward_demodulation, [], [f156, f834])).
fof(f156, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f1842, plain, (~ spl24_28 | ~ spl24_3), inference(avatar_split_clause, [], [f1663, f353, f458])).
fof(f458, plain, (spl24_28 <=> (e2 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_28])])).
fof(f1663, plain, (~ (e2 = op(e3, e4)) | ~ spl24_3), inference(forward_demodulation, [], [f194, f355])).
fof(f194, plain, ~ (op(e3, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1829, plain, (~ spl24_13 | ~ spl24_3), inference(avatar_split_clause, [], [f1646, f353, f395])).
fof(f395, plain, (spl24_13 <=> (e2 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_13])])).
fof(f1646, plain, (~ (e2 = op(e4, e2)) | ~ spl24_3), inference(forward_demodulation, [], [f243, f355])).
fof(f243, plain, ~ (op(e4, e2) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1770, plain, (~ spl24_58 | ~ spl24_73), inference(avatar_split_clause, [], [f1768, f647, f584])).
fof(f584, plain, (spl24_58 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_58])])).
fof(f1768, plain, (~ (e2 = op(e2, e3)) | ~ spl24_73), inference(backward_demodulation, [], [f217, f649])).
fof(f217, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f1769, plain, (~ spl24_48 | ~ spl24_73), inference(avatar_split_clause, [], [f1767, f647, f542])).
fof(f542, plain, (spl24_48 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_48])])).
fof(f1767, plain, (~ (e2 = op(e3, e0)) | ~ spl24_73), inference(backward_demodulation, [], [f152, f649])).
fof(f152, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f1764, plain, (~ spl24_33 | ~ spl24_83), inference(avatar_split_clause, [], [f1762, f689, f479])).
fof(f479, plain, (spl24_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_33])])).
fof(f1762, plain, (~ (e2 = op(e3, e3)) | ~ spl24_83), inference(backward_demodulation, [], [f180, f691])).
fof(f180, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f1758, plain, (~ spl24_82 | ~ spl24_97), inference(avatar_split_clause, [], [f1755, f748, f685])).
fof(f685, plain, (spl24_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_82])])).
fof(f1755, plain, (~ (e1 = op(e1, e3)) | ~ spl24_97), inference(backward_demodulation, [], [f207, f750])).
fof(f207, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1756, plain, (~ spl24_72 | ~ spl24_97), inference(avatar_split_clause, [], [f1753, f748, f643])).
fof(f643, plain, (spl24_72 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_72])])).
fof(f1753, plain, (~ (e1 = op(e2, e0)) | ~ spl24_97), inference(backward_demodulation, [], [f149, f750])).
fof(f149, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f1749, plain, (~ spl24_84 | ~ spl24_109), inference(avatar_split_clause, [], [f1746, f798, f693])).
fof(f693, plain, (spl24_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_84])])).
fof(f1746, plain, (~ (e3 = op(e1, e3)) | ~ spl24_109), inference(backward_demodulation, [], [f175, f800])).
fof(f175, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1744, plain, (~ spl24_38 | ~ spl24_113), inference(avatar_split_clause, [], [f1742, f815, f500])).
fof(f500, plain, (spl24_38 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_38])])).
fof(f1742, plain, (~ (e2 = op(e3, e2)) | ~ spl24_113), inference(backward_demodulation, [], [f167, f817])).
fof(f167, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f1674, plain, (~ spl24_41 | ~ spl24_26), inference(avatar_split_clause, [], [f1673, f450, f513])).
fof(f513, plain, (spl24_41 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_41])])).
fof(f450, plain, (spl24_26 <=> (e0 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_26])])).
fof(f1673, plain, (~ (e0 = op(e3, e1)) | ~ spl24_26), inference(forward_demodulation, [], [f231, f452])).
fof(f452, plain, ((e0 = op(e3, e4)) | ~ spl24_26), inference(avatar_component_clause, [], [f450])).
fof(f231, plain, ~ (op(e3, e1) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1662, plain, (~ spl24_6 | ~ spl24_26 | spl24_134), inference(avatar_contradiction_clause, [], [f1661])).
fof(f1661, plain, ($false | (~ spl24_6 | ~ spl24_26 | spl24_134)), inference(subsumption_resolution, [], [f1660, f452])).
fof(f1660, plain, (~ (e0 = op(e3, e4)) | (~ spl24_6 | spl24_134)), inference(forward_demodulation, [], [f962, f368])).
fof(f368, plain, ((e0 = op(e4, e3)) | ~ spl24_6), inference(avatar_component_clause, [], [f366])).
fof(f366, plain, (spl24_6 <=> (e0 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_6])])).
fof(f962, plain, (~ (op(e3, e4) = op(e4, e3)) | spl24_134), inference(avatar_component_clause, [], [f960])).
fof(f960, plain, (spl24_134 <=> (op(e3, e4) = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_134])])).
fof(f1636, plain, (~ spl24_2 | ~ spl24_3), inference(avatar_contradiction_clause, [], [f1635])).
fof(f1635, plain, ($false | (~ spl24_2 | ~ spl24_3)), inference(subsumption_resolution, [], [f1634, f249])).
fof(f1634, plain, ((e1 = e2) | (~ spl24_2 | ~ spl24_3)), inference(backward_demodulation, [], [f355, f351])).
fof(f351, plain, ((e1 = op(e4, e4)) | ~ spl24_2), inference(avatar_component_clause, [], [f349])).
fof(f349, plain, (spl24_2 <=> (e1 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_2])])).
fof(f1588, plain, (~ spl24_24 | ~ spl24_25), inference(avatar_contradiction_clause, [], [f1587])).
fof(f1587, plain, ($false | (~ spl24_24 | ~ spl24_25)), inference(subsumption_resolution, [], [f1586, f254])).
fof(f254, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f1586, plain, ((e3 = e4) | (~ spl24_24 | ~ spl24_25)), inference(backward_demodulation, [], [f447, f443])).
fof(f443, plain, ((e3 = op(e4, e0)) | ~ spl24_24), inference(avatar_component_clause, [], [f441])).
fof(f441, plain, (spl24_24 <=> (e3 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_24])])).
fof(f1583, plain, (~ spl24_15 | ~ spl24_25), inference(avatar_split_clause, [], [f1579, f445, f403])).
fof(f403, plain, (spl24_15 <=> (e4 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_15])])).
fof(f1579, plain, (~ (e4 = op(e4, e2)) | ~ spl24_25), inference(backward_demodulation, [], [f236, f447])).
fof(f236, plain, ~ (op(e4, e0) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1516, plain, (~ spl24_59 | ~ spl24_64), inference(avatar_split_clause, [], [f1512, f609, f588])).
fof(f588, plain, (spl24_59 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_59])])).
fof(f1512, plain, (~ (e3 = op(e2, e3)) | ~ spl24_64), inference(backward_demodulation, [], [f222, f611])).
fof(f222, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f1515, plain, (~ spl24_14 | ~ spl24_64), inference(avatar_split_clause, [], [f1511, f609, f399])).
fof(f399, plain, (spl24_14 <=> (e3 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_14])])).
fof(f1511, plain, (~ (e3 = op(e4, e2)) | ~ spl24_64), inference(backward_demodulation, [], [f173, f611])).
fof(f173, plain, ~ (op(e2, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1514, plain, (~ spl24_39 | ~ spl24_64), inference(avatar_split_clause, [], [f1510, f609, f504])).
fof(f504, plain, (spl24_39 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_39])])).
fof(f1510, plain, (~ (e3 = op(e3, e2)) | ~ spl24_64), inference(backward_demodulation, [], [f172, f611])).
fof(f172, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f1508, plain, (~ spl24_56 | ~ spl24_66), inference(avatar_split_clause, [], [f1503, f618, f576])).
fof(f576, plain, (spl24_56 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_56])])).
fof(f1503, plain, (~ (e0 = op(e2, e3)) | ~ spl24_66), inference(backward_demodulation, [], [f220, f620])).
fof(f220, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f1481, plain, (~ spl24_4 | ~ spl24_79), inference(avatar_split_clause, [], [f1478, f672, f357])).
fof(f357, plain, (spl24_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_4])])).
fof(f1478, plain, (~ (e3 = op(e4, e4)) | ~ spl24_79), inference(backward_demodulation, [], [f191, f674])).
fof(f1467, plain, (~ spl24_76 | ~ spl24_86), inference(avatar_split_clause, [], [f1462, f702, f660])).
fof(f660, plain, (spl24_76 <=> (e0 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_76])])).
fof(f1462, plain, (~ (e0 = op(e1, e4)) | ~ spl24_86), inference(backward_demodulation, [], [f213, f704])).
fof(f213, plain, ~ (op(e1, e2) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f1466, plain, (~ spl24_81 | ~ spl24_86), inference(avatar_split_clause, [], [f1461, f702, f681])).
fof(f681, plain, (spl24_81 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_81])])).
fof(f1461, plain, (~ (e0 = op(e1, e3)) | ~ spl24_86), inference(backward_demodulation, [], [f212, f704])).
fof(f212, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1465, plain, (~ spl24_11 | ~ spl24_86), inference(avatar_split_clause, [], [f1460, f702, f387])).
fof(f387, plain, (spl24_11 <=> (e0 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_11])])).
fof(f1460, plain, (~ (e0 = op(e4, e2)) | ~ spl24_86), inference(backward_demodulation, [], [f171, f704])).
fof(f171, plain, ~ (op(e1, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1464, plain, (~ spl24_36 | ~ spl24_86), inference(avatar_split_clause, [], [f1459, f702, f492])).
fof(f492, plain, (spl24_36 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_36])])).
fof(f1459, plain, (~ (e0 = op(e3, e2)) | ~ spl24_86), inference(backward_demodulation, [], [f170, f704])).
fof(f170, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f1463, plain, (~ spl24_61 | ~ spl24_86), inference(avatar_split_clause, [], [f1458, f702, f597])).
fof(f597, plain, (spl24_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_61])])).
fof(f1458, plain, (~ (e0 = op(e2, e2)) | ~ spl24_86), inference(backward_demodulation, [], [f169, f704])).
fof(f169, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f1457, plain, (spl24_3 | ~ spl24_95), inference(avatar_split_clause, [], [f1450, f739, f353])).
fof(f1450, plain, ((e2 = op(e4, e4)) | ~ spl24_95), inference(backward_demodulation, [], [f256, f741])).
fof(f256, plain, (e2 = op(op(e1, e1), op(e1, e1))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e4 = op(e1, e1)) & (e3 = op(op(op(e1, e1), op(e1, e1)), op(op(e1, e1), op(e1, e1)))) & (e2 = op(op(e1, e1), op(e1, e1))) & (e0 = op(e1, op(op(e1, e1), op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG064+1.p', ax6)).
fof(f1456, plain, (~ spl24_80 | ~ spl24_95), inference(avatar_split_clause, [], [f1449, f739, f676])).
fof(f676, plain, (spl24_80 <=> (e4 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_80])])).
fof(f1449, plain, (~ (e4 = op(e1, e4)) | ~ spl24_95), inference(backward_demodulation, [], [f211, f741])).
fof(f211, plain, ~ (op(e1, e1) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f1455, plain, (~ spl24_85 | ~ spl24_95), inference(avatar_split_clause, [], [f1448, f739, f697])).
fof(f697, plain, (spl24_85 <=> (e4 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_85])])).
fof(f1448, plain, (~ (e4 = op(e1, e3)) | ~ spl24_95), inference(backward_demodulation, [], [f210, f741])).
fof(f210, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1410, plain, (~ spl24_31 | ~ spl24_106), inference(avatar_split_clause, [], [f1405, f786, f471])).
fof(f471, plain, (spl24_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_31])])).
fof(f1405, plain, (~ (e0 = op(e3, e3)) | ~ spl24_106), inference(backward_demodulation, [], [f177, f788])).
fof(f788, plain, ((e0 = op(e0, e3)) | ~ spl24_106), inference(avatar_component_clause, [], [f786])).
fof(f177, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f1376, plain, (~ spl24_101 | ~ spl24_121), inference(avatar_split_clause, [], [f1368, f849, f765])).
fof(f765, plain, (spl24_101 <=> (e0 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_101])])).
fof(f1368, plain, (~ (e0 = op(e0, e4)) | ~ spl24_121), inference(backward_demodulation, [], [f198, f851])).
fof(f198, plain, ~ (op(e0, e0) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f1373, plain, (~ spl24_116 | ~ spl24_121), inference(avatar_split_clause, [], [f1365, f849, f828])).
fof(f828, plain, (spl24_116 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_116])])).
fof(f1365, plain, (~ (e0 = op(e0, e1)) | ~ spl24_121), inference(backward_demodulation, [], [f195, f851])).
fof(f195, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f4])).
fof(f1356, plain, (spl24_73 | ~ spl24_126), inference(avatar_split_clause, [], [f1346, f870, f647])).
fof(f1346, plain, ((e2 = op(e2, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f89, f872])).
fof(f89, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f1355, plain, (spl24_113 | ~ spl24_126), inference(avatar_split_clause, [], [f1345, f870, f815])).
fof(f1345, plain, ((e2 = op(e0, e2)) | ~ spl24_126), inference(backward_demodulation, [], [f88, f872])).
fof(f1353, plain, (spl24_117 | ~ spl24_126), inference(avatar_split_clause, [], [f1343, f870, f832])).
fof(f1343, plain, ((e1 = op(e0, e1)) | ~ spl24_126), inference(backward_demodulation, [], [f86, f872])).
fof(f86, plain, (e1 = op(unit, e1)), inference(cnf_transformation, [], [f2])).
fof(f1340, plain, (spl24_121 | spl24_91 | spl24_61 | spl24_31 | spl24_1), inference(avatar_split_clause, [], [f331, f345, f471, f597, f723, f849])).
fof(f331, plain, ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(definition_folding, [], [f9, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
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
fof(f9, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG064+1.p', co1)).
fof(f1339, plain, (spl24_122 | spl24_92 | spl24_62 | spl24_32 | spl24_2), inference(avatar_split_clause, [], [f332, f349, f475, f601, f727, f853])).
fof(f332, plain, ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f34])).
fof(f1335, plain, (spl24_207 | spl24_204 | spl24_201 | spl24_198 | spl24_195 | spl24_191 | spl24_189 | spl24_186 | spl24_183 | spl24_180 | spl24_176 | spl24_172 | spl24_170 | spl24_167 | spl24_164 | spl24_160 | spl24_156 | spl24_152 | spl24_150 | spl24_147 | spl24_143 | spl24_139 | spl24_135 | spl24_131), inference(avatar_split_clause, [], [f339, f946, f965, f984, f1003, f1022, f1037, f1048, f1067, f1086, f1105, f1120, f1135, f1146, f1165, f1184, f1199, f1214, f1229, f1240, f1259, f1274, f1289, f1304, f1319])).
fof(f1319, plain, (spl24_207 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl24_207])])).
fof(f1304, plain, (spl24_204 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl24_204])])).
fof(f1289, plain, (spl24_201 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl24_201])])).
fof(f1274, plain, (spl24_198 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl24_198])])).
fof(f1259, plain, (spl24_195 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl24_195])])).
fof(f1240, plain, (spl24_191 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl24_191])])).
fof(f1229, plain, (spl24_189 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl24_189])])).
fof(f1214, plain, (spl24_186 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl24_186])])).
fof(f1199, plain, (spl24_183 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl24_183])])).
fof(f1184, plain, (spl24_180 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl24_180])])).
fof(f1165, plain, (spl24_176 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl24_176])])).
fof(f1146, plain, (spl24_172 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl24_172])])).
fof(f1135, plain, (spl24_170 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl24_170])])).
fof(f1120, plain, (spl24_167 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl24_167])])).
fof(f1105, plain, (spl24_164 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl24_164])])).
fof(f1086, plain, (spl24_160 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl24_160])])).
fof(f1067, plain, (spl24_156 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl24_156])])).
fof(f1048, plain, (spl24_152 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl24_152])])).
fof(f1037, plain, (spl24_150 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl24_150])])).
fof(f1022, plain, (spl24_147 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl24_147])])).
fof(f1003, plain, (spl24_143 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl24_143])])).
fof(f984, plain, (spl24_139 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl24_139])])).
fof(f965, plain, (spl24_135 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl24_135])])).
fof(f946, plain, (spl24_131 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl24_131])])).
fof(f339, plain, (sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(trivial_inequality_removal, [], [f336])).
fof(f336, plain, (~ (op(e4, e4) = op(e4, e4)) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f34])).
fof(f1328, plain, ~ spl24_207, inference(avatar_split_clause, [], [f340, f1319])).
fof(f340, plain, ~ sP0, inference(trivial_inequality_removal, [], [f328])).
fof(f328, plain, (~ (op(e0, e0) = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f1317, plain, (~ spl24_204 | ~ spl24_194), inference(avatar_split_clause, [], [f325, f1254, f1304])).
fof(f325, plain, (~ (op(e0, e1) = op(e1, e0)) | ~ sP1), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f1302, plain, (~ spl24_201 | ~ spl24_179), inference(avatar_split_clause, [], [f322, f1179, f1289])).
fof(f322, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP2), inference(nnf_transformation, [], [f12])).
fof(f1287, plain, (~ spl24_198 | ~ spl24_163), inference(avatar_split_clause, [], [f319, f1100, f1274])).
fof(f319, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP3), inference(nnf_transformation, [], [f13])).
fof(f1272, plain, (~ spl24_195 | ~ spl24_146), inference(avatar_split_clause, [], [f316, f1017, f1259])).
fof(f316, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP4), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP4), inference(nnf_transformation, [], [f14])).
fof(f1257, plain, (~ spl24_191 | ~ spl24_194), inference(avatar_split_clause, [], [f313, f1254, f1240])).
fof(f313, plain, (~ (op(e0, e1) = op(e1, e0)) | ~ sP5), inference(cnf_transformation, [], [f53])).
fof(f53, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP5), inference(nnf_transformation, [], [f15])).
fof(f1238, plain, ~ spl24_189, inference(avatar_split_clause, [], [f341, f1229])).
fof(f341, plain, ~ sP6, inference(trivial_inequality_removal, [], [f310])).
fof(f310, plain, (~ (op(e1, e1) = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f1227, plain, (~ spl24_186 | ~ spl24_175), inference(avatar_split_clause, [], [f307, f1160, f1214])).
fof(f307, plain, (~ (op(e1, e2) = op(e2, e1)) | ~ sP7), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP7), inference(nnf_transformation, [], [f17])).
fof(f1212, plain, (~ spl24_183 | ~ spl24_159), inference(avatar_split_clause, [], [f304, f1081, f1199])).
fof(f304, plain, (~ (op(e1, e3) = op(e3, e1)) | ~ sP8), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP8), inference(nnf_transformation, [], [f18])).
fof(f1197, plain, (~ spl24_180 | ~ spl24_142), inference(avatar_split_clause, [], [f301, f998, f1184])).
fof(f301, plain, (~ (op(e1, e4) = op(e4, e1)) | ~ sP9), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP9), inference(nnf_transformation, [], [f19])).
fof(f1182, plain, (~ spl24_176 | ~ spl24_179), inference(avatar_split_clause, [], [f298, f1179, f1165])).
fof(f298, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP10), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP10), inference(nnf_transformation, [], [f20])).
fof(f1158, plain, (~ spl24_172 | spl24_174), inference(avatar_split_clause, [], [f296, f1155, f1146])).
fof(f296, plain, ((e2 = op(op(e2, e1), e1)) | ~ sP11), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP11), inference(nnf_transformation, [], [f21])).
fof(f1144, plain, ~ spl24_170, inference(avatar_split_clause, [], [f342, f1135])).
fof(f342, plain, ~ sP12, inference(trivial_inequality_removal, [], [f292])).
fof(f292, plain, (~ (op(e2, e2) = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP12), inference(nnf_transformation, [], [f22])).
fof(f1133, plain, (~ spl24_167 | ~ spl24_155), inference(avatar_split_clause, [], [f289, f1062, f1120])).
fof(f289, plain, (~ (op(e2, e3) = op(e3, e2)) | ~ sP13), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP13), inference(nnf_transformation, [], [f23])).
fof(f1118, plain, (~ spl24_164 | ~ spl24_138), inference(avatar_split_clause, [], [f286, f979, f1105])).
fof(f286, plain, (~ (op(e2, e4) = op(e4, e2)) | ~ sP14), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP14), inference(nnf_transformation, [], [f24])).
fof(f1103, plain, (~ spl24_160 | ~ spl24_163), inference(avatar_split_clause, [], [f283, f1100, f1086])).
fof(f283, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP15), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP15), inference(nnf_transformation, [], [f25])).
fof(f1084, plain, (~ spl24_156 | ~ spl24_159), inference(avatar_split_clause, [], [f280, f1081, f1067])).
fof(f280, plain, (~ (op(e1, e3) = op(e3, e1)) | ~ sP16), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP16), inference(nnf_transformation, [], [f26])).
fof(f1065, plain, (~ spl24_152 | ~ spl24_155), inference(avatar_split_clause, [], [f277, f1062, f1048])).
fof(f277, plain, (~ (op(e2, e3) = op(e3, e2)) | ~ sP17), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP17), inference(nnf_transformation, [], [f27])).
fof(f1046, plain, ~ spl24_150, inference(avatar_split_clause, [], [f343, f1037])).
fof(f343, plain, ~ sP18, inference(trivial_inequality_removal, [], [f274])).
fof(f274, plain, (~ (op(e3, e3) = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP18), inference(nnf_transformation, [], [f28])).
fof(f1035, plain, (~ spl24_147 | ~ spl24_134), inference(avatar_split_clause, [], [f271, f960, f1022])).
fof(f271, plain, (~ (op(e3, e4) = op(e4, e3)) | ~ sP19), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP19), inference(nnf_transformation, [], [f29])).
fof(f1020, plain, (~ spl24_143 | ~ spl24_146), inference(avatar_split_clause, [], [f268, f1017, f1003])).
fof(f268, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP20), inference(cnf_transformation, [], [f38])).
fof(f38, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP20), inference(nnf_transformation, [], [f30])).
fof(f996, plain, (~ spl24_139 | spl24_141), inference(avatar_split_clause, [], [f266, f993, f984])).
fof(f266, plain, ((e4 = op(op(e4, e1), e1)) | ~ sP21), inference(cnf_transformation, [], [f37])).
fof(f37, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP21), inference(nnf_transformation, [], [f31])).
fof(f977, plain, (~ spl24_135 | spl24_137), inference(avatar_split_clause, [], [f263, f974, f965])).
fof(f263, plain, ((e4 = op(op(e4, e2), e2)) | ~ sP22), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP22), inference(nnf_transformation, [], [f32])).
fof(f963, plain, (~ spl24_131 | ~ spl24_134), inference(avatar_split_clause, [], [f259, f960, f946])).
fof(f259, plain, (~ (op(e3, e4) = op(e4, e3)) | ~ sP23), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP23), inference(nnf_transformation, [], [f33])).
fof(f944, plain, spl24_86, inference(avatar_split_clause, [], [f943, f702])).
fof(f943, plain, (e0 = op(e1, e2)), inference(forward_demodulation, [], [f255, f256])).
fof(f255, plain, (e0 = op(e1, op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f6])).
fof(f942, plain, spl24_64, inference(avatar_split_clause, [], [f941, f609])).
fof(f941, plain, (e3 = op(e2, e2)), inference(backward_demodulation, [], [f257, f256])).
fof(f257, plain, (e3 = op(op(op(e1, e1), op(e1, e1)), op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f6])).
fof(f940, plain, spl24_95, inference(avatar_split_clause, [], [f258, f739])).
fof(f258, plain, (e4 = op(e1, e1)), inference(cnf_transformation, [], [f6])).
fof(f928, plain, (spl24_116 | spl24_91 | spl24_66 | spl24_41 | spl24_16), inference(avatar_split_clause, [], [f106, f408, f513, f618, f723, f828])).
fof(f106, plain, ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG064+1.p', ax3)).
fof(f917, plain, (spl24_72 | spl24_67 | spl24_62 | spl24_57 | spl24_52), inference(avatar_split_clause, [], [f117, f559, f580, f601, f622, f643])).
fof(f117, plain, ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f905, plain, (spl24_48 | spl24_43 | spl24_38 | spl24_33 | spl24_28), inference(avatar_split_clause, [], [f129, f458, f479, f500, f521, f542])).
fof(f129, plain, ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f899, plain, (spl24_21 | spl24_16 | spl24_11 | spl24_6 | spl24_1), inference(avatar_split_clause, [], [f135, f345, f366, f387, f408, f429])).
fof(f135, plain, ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f898, plain, (spl24_101 | spl24_76 | spl24_51 | spl24_26 | spl24_1), inference(avatar_split_clause, [], [f136, f345, f450, f555, f660, f765])).
fof(f136, plain, ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f893, plain, (spl24_24 | spl24_19 | spl24_14 | spl24_9 | spl24_4), inference(avatar_split_clause, [], [f141, f357, f378, f399, f420, f441])).
fof(f141, plain, ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f889, plain, (spl24_126 | spl24_127 | spl24_128 | spl24_129 | spl24_130), inference(avatar_split_clause, [], [f94, f886, f882, f878, f874, f870])).
fof(f94, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f700, plain, (spl24_81 | spl24_82 | spl24_83 | spl24_84 | spl24_85), inference(avatar_split_clause, [], [f67, f697, f693, f689, f685, f681])).
fof(f67, plain, ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG064+1.p', ax1)).
fof(f679, plain, (spl24_76 | spl24_77 | spl24_78 | spl24_79 | spl24_80), inference(avatar_split_clause, [], [f68, f676, f672, f668, f664, f660])).
fof(f68, plain, ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))), inference(cnf_transformation, [], [f1])).
fof(f595, plain, (spl24_56 | spl24_57 | spl24_58 | spl24_59 | spl24_60), inference(avatar_split_clause, [], [f72, f592, f588, f584, f580, f576])).
fof(f72, plain, ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f511, plain, (spl24_36 | spl24_37 | spl24_38 | spl24_39 | spl24_40), inference(avatar_split_clause, [], [f76, f508, f504, f500, f496, f492])).
fof(f76, plain, ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f406, plain, (spl24_11 | spl24_12 | spl24_13 | spl24_14 | spl24_15), inference(avatar_split_clause, [], [f81, f403, f399, f395, f391, f387])).
fof(f81, plain, ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))), inference(cnf_transformation, [], [f1])).