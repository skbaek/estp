fof(f2594, plain, $false, inference(avatar_sat_refutation, [], [f245, f296, f313, f330, f347, f364, f398, f415, f432, f449, f466, f483, f500, f501, f503, f504, f506, f507, f508, f516, f518, f519, f520, f524, f525, f527, f537, f538, f543, f544, f549, f550, f555, f556, f560, f564, f568, f570, f572, f576, f578, f579, f580, f584, f587, f588, f597, f606, f620, f630, f657, f673, f678, f687, f688, f689, f706, f707, f721, f722, f723, f733, f735, f795, f798, f806, f810, f835, f837, f840, f850, f853, f856, f863, f870, f887, f891, f898, f914, f924, f950, f981, f986, f997, f1019, f1041, f1053, f1079, f1087, f1092, f1102, f1114, f1116, f1159, f1178, f1211, f1222, f1223, f1232, f1250, f1267, f1276, f1326, f1384, f1408, f1479, f1489, f1503, f1588, f1589, f1599, f1611, f1614, f1653, f1655, f1661, f1663, f1672, f1674, f1677, f1680, f1709, f1730, f1762, f1804, f1817, f1822, f1834, f1871, f1876, f1877, f1900, f1902, f1903, f1927, f1928, f1940, f1978, f1985, f1989, f2037, f2038, f2039, f2054, f2063, f2067, f2124, f2126, f2151, f2156, f2158, f2173, f2178, f2179, f2186, f2210, f2218, f2250, f2260, f2281, f2292, f2304, f2311, f2350, f2390, f2399, f2405, f2412, f2421, f2422, f2423, f2432, f2439, f2440, f2443, f2449, f2462, f2495, f2498, f2515, f2516, f2517, f2522, f2539, f2540, f2557, f2558, f2562, f2569, f2593])).
fof(f2593, plain, (~ spl4_1 | ~ spl4_24 | ~ spl4_50 | spl4_70), inference(avatar_contradiction_clause, [], [f2592])).
fof(f2592, plain, ($false | (~ spl4_1 | ~ spl4_24 | ~ spl4_50 | spl4_70)), inference(subsumption_resolution, [], [f2587, f440])).
fof(f440, plain, ((e1 = op(e0, e3)) | ~ spl4_50), inference(avatar_component_clause, [], [f438])).
fof(f438, plain, (spl4_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_50])])).
fof(f2587, plain, (~ (e1 = op(e0, e3)) | (~ spl4_1 | ~ spl4_24 | spl4_70)), inference(backward_demodulation, [], [f2565, f232])).
fof(f232, plain, ((e0 = op(e3, e3)) | ~ spl4_1), inference(avatar_component_clause, [], [f230])).
fof(f230, plain, (spl4_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_1])])).
fof(f2565, plain, (~ (e1 = op(op(e3, e3), e3)) | (~ spl4_24 | spl4_70)), inference(backward_demodulation, [], [f596, f329])).
fof(f329, plain, ((e3 = op(e2, e2)) | ~ spl4_24), inference(avatar_component_clause, [], [f327])).
fof(f327, plain, (spl4_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_24])])).
fof(f596, plain, (~ (e1 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) | spl4_70), inference(avatar_component_clause, [], [f594])).
fof(f594, plain, (spl4_70 <=> (e1 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_70])])).
fof(f2569, plain, (~ spl4_1 | ~ spl4_24 | spl4_69), inference(avatar_split_clause, [], [f2564, f590, f327, f230])).
fof(f590, plain, (spl4_69 <=> (e0 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_69])])).
fof(f2564, plain, (~ (e0 = op(e3, e3)) | (~ spl4_24 | spl4_69)), inference(backward_demodulation, [], [f592, f329])).
fof(f592, plain, (~ (e0 = op(op(e2, e2), op(e2, e2))) | spl4_69), inference(avatar_component_clause, [], [f590])).
fof(f2562, plain, (~ spl4_22 | ~ spl4_26), inference(avatar_split_clause, [], [f2559, f336, f319])).
fof(f319, plain, (spl4_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_22])])).
fof(f336, plain, (spl4_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_26])])).
fof(f2559, plain, (~ (e1 = op(e2, e2)) | ~ spl4_26), inference(backward_demodulation, [], [f150, f338])).
fof(f338, plain, ((e1 = op(e2, e1)) | ~ spl4_26), inference(avatar_component_clause, [], [f336])).
fof(f150, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax3)).
fof(f2558, plain, (~ spl4_25 | ~ spl4_29), inference(avatar_split_clause, [], [f2552, f349, f332])).
fof(f332, plain, (spl4_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_25])])).
fof(f349, plain, (spl4_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_29])])).
fof(f2552, plain, (~ (e0 = op(e2, e1)) | ~ spl4_29), inference(backward_demodulation, [], [f147, f351])).
fof(f351, plain, ((e0 = op(e2, e0)) | ~ spl4_29), inference(avatar_component_clause, [], [f349])).
fof(f147, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2557, plain, (~ spl4_13 | ~ spl4_29), inference(avatar_split_clause, [], [f2551, f349, f281])).
fof(f281, plain, (spl4_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_13])])).
fof(f2551, plain, (~ (e0 = op(e3, e0)) | ~ spl4_29), inference(backward_demodulation, [], [f116, f351])).
fof(f116, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2540, plain, (~ spl4_33 | ~ spl4_41), inference(avatar_split_clause, [], [f2531, f400, f366])).
fof(f366, plain, (spl4_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_33])])).
fof(f400, plain, (spl4_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_41])])).
fof(f2531, plain, (~ (e0 = op(e1, e3)) | ~ spl4_41), inference(backward_demodulation, [], [f145, f402])).
fof(f402, plain, ((e0 = op(e1, e1)) | ~ spl4_41), inference(avatar_component_clause, [], [f400])).
fof(f145, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2539, plain, (~ spl4_25 | ~ spl4_41), inference(avatar_split_clause, [], [f2529, f400, f332])).
fof(f2529, plain, (~ (e0 = op(e2, e1)) | ~ spl4_41), inference(backward_demodulation, [], [f120, f402])).
fof(f120, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2522, plain, (~ spl4_43 | ~ spl4_59), inference(avatar_split_clause, [], [f2518, f476, f408])).
fof(f408, plain, (spl4_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_43])])).
fof(f476, plain, (spl4_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_59])])).
fof(f2518, plain, (~ (e2 = op(e1, e1)) | ~ spl4_59), inference(backward_demodulation, [], [f117, f478])).
fof(f478, plain, ((e2 = op(e0, e1)) | ~ spl4_59), inference(avatar_component_clause, [], [f476])).
fof(f117, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2517, plain, (~ spl4_2 | ~ spl4_64 | spl4_82), inference(avatar_split_clause, [], [f2511, f650, f497, f234])).
fof(f234, plain, (spl4_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_2])])).
fof(f497, plain, (spl4_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl4_64])])).
fof(f650, plain, (spl4_82 <=> (e1 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_82])])).
fof(f2511, plain, (~ (e1 = op(e3, e3)) | (~ spl4_64 | spl4_82)), inference(backward_demodulation, [], [f652, f499])).
fof(f499, plain, ((op(e0, e0) = e3) | ~ spl4_64), inference(avatar_component_clause, [], [f497])).
fof(f652, plain, (~ (e1 = op(op(e0, e0), op(e0, e0))) | spl4_82), inference(avatar_component_clause, [], [f650])).
fof(f2516, plain, (~ spl4_52 | ~ spl4_64), inference(avatar_split_clause, [], [f2510, f497, f446])).
fof(f446, plain, (spl4_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_52])])).
fof(f2510, plain, (~ (e3 = op(e0, e3)) | ~ spl4_64), inference(backward_demodulation, [], [f137, f499])).
fof(f137, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2515, plain, (~ spl4_32 | ~ spl4_64), inference(avatar_split_clause, [], [f2507, f497, f361])).
fof(f361, plain, (spl4_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_32])])).
fof(f2507, plain, (~ (e3 = op(e2, e0)) | ~ spl4_64), inference(backward_demodulation, [], [f112, f499])).
fof(f112, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2498, plain, (~ spl4_14 | ~ spl4_46), inference(avatar_split_clause, [], [f2497, f421, f285])).
fof(f285, plain, (spl4_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_14])])).
fof(f421, plain, (spl4_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_46])])).
fof(f2497, plain, (~ (e1 = op(e3, e0)) | ~ spl4_46), inference(forward_demodulation, [], [f115, f423])).
fof(f423, plain, ((e1 = op(e1, e0)) | ~ spl4_46), inference(avatar_component_clause, [], [f421])).
fof(f115, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2495, plain, (~ spl4_3 | ~ spl4_19), inference(avatar_split_clause, [], [f2494, f306, f238])).
fof(f238, plain, (spl4_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_3])])).
fof(f306, plain, (spl4_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_19])])).
fof(f2494, plain, (~ (e2 = op(e3, e3)) | ~ spl4_19), inference(forward_demodulation, [], [f134, f308])).
fof(f308, plain, ((e2 = op(e2, e3)) | ~ spl4_19), inference(avatar_component_clause, [], [f306])).
fof(f134, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2462, plain, (~ spl4_25 | ~ spl4_2 | ~ spl4_43 | spl4_81), inference(avatar_split_clause, [], [f2458, f645, f408, f234, f332])).
fof(f645, plain, (spl4_81 <=> (e0 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_81])])).
fof(f2458, plain, (~ (e0 = op(e2, e1)) | (~ spl4_2 | ~ spl4_43 | spl4_81)), inference(backward_demodulation, [], [f2385, f410])).
fof(f410, plain, ((e2 = op(e1, e1)) | ~ spl4_43), inference(avatar_component_clause, [], [f408])).
fof(f2385, plain, (~ (e0 = op(op(e1, e1), e1)) | (~ spl4_2 | spl4_81)), inference(forward_demodulation, [], [f647, f236])).
fof(f236, plain, ((e1 = op(e3, e3)) | ~ spl4_2), inference(avatar_component_clause, [], [f234])).
fof(f647, plain, (~ (e0 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | spl4_81), inference(avatar_component_clause, [], [f645])).
fof(f2449, plain, (~ spl4_38 | ~ spl4_46), inference(avatar_split_clause, [], [f2446, f421, f387])).
fof(f387, plain, (spl4_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_38])])).
fof(f2446, plain, (~ (e1 = op(e1, e2)) | ~ spl4_46), inference(backward_demodulation, [], [f142, f423])).
fof(f142, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2443, plain, (~ spl4_20 | ~ spl4_52), inference(avatar_split_clause, [], [f2442, f446, f310])).
fof(f310, plain, (spl4_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_20])])).
fof(f2442, plain, (~ (e3 = op(e2, e3)) | ~ spl4_52), inference(backward_demodulation, [], [f130, f448])).
fof(f448, plain, ((e3 = op(e0, e3)) | ~ spl4_52), inference(avatar_component_clause, [], [f446])).
fof(f130, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2440, plain, (~ spl4_49 | ~ spl4_53), inference(avatar_split_clause, [], [f2435, f451, f434])).
fof(f434, plain, (spl4_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_49])])).
fof(f451, plain, (spl4_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_53])])).
fof(f2435, plain, (~ (e0 = op(e0, e3)) | ~ spl4_53), inference(backward_demodulation, [], [f140, f453])).
fof(f453, plain, ((e0 = op(e0, e2)) | ~ spl4_53), inference(avatar_component_clause, [], [f451])).
fof(f140, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2439, plain, (~ spl4_21 | ~ spl4_53), inference(avatar_split_clause, [], [f2434, f451, f315])).
fof(f315, plain, (spl4_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_21])])).
fof(f2434, plain, (~ (e0 = op(e2, e2)) | ~ spl4_53), inference(backward_demodulation, [], [f124, f453])).
fof(f124, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2432, plain, (~ spl4_26 | ~ spl4_58), inference(avatar_split_clause, [], [f2427, f472, f336])).
fof(f472, plain, (spl4_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_58])])).
fof(f2427, plain, (~ (e1 = op(e2, e1)) | ~ spl4_58), inference(backward_demodulation, [], [f118, f474])).
fof(f474, plain, ((e1 = op(e0, e1)) | ~ spl4_58), inference(avatar_component_clause, [], [f472])).
fof(f118, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2423, plain, (spl4_22 | ~ spl4_63 | ~ spl4_82), inference(avatar_split_clause, [], [f2419, f650, f493, f319])).
fof(f493, plain, (spl4_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl4_63])])).
fof(f2419, plain, ((e1 = op(e2, e2)) | (~ spl4_63 | ~ spl4_82)), inference(backward_demodulation, [], [f651, f495])).
fof(f495, plain, ((op(e0, e0) = e2) | ~ spl4_63), inference(avatar_component_clause, [], [f493])).
fof(f651, plain, ((e1 = op(op(e0, e0), op(e0, e0))) | ~ spl4_82), inference(avatar_component_clause, [], [f650])).
fof(f2422, plain, (~ spl4_59 | ~ spl4_63), inference(avatar_split_clause, [], [f2415, f493, f476])).
fof(f2415, plain, (~ (e2 = op(e0, e1)) | ~ spl4_63), inference(backward_demodulation, [], [f135, f495])).
fof(f135, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f2421, plain, (~ spl4_31 | ~ spl4_63), inference(avatar_split_clause, [], [f2414, f493, f357])).
fof(f357, plain, (spl4_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_31])])).
fof(f2414, plain, (~ (e2 = op(e2, e0)) | ~ spl4_63), inference(backward_demodulation, [], [f112, f495])).
fof(f2412, plain, (~ spl4_41 | ~ spl4_2 | spl4_71), inference(avatar_split_clause, [], [f2411, f599, f234, f400])).
fof(f599, plain, (spl4_71 <=> (e0 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_71])])).
fof(f2411, plain, (~ (e0 = op(e1, e1)) | (~ spl4_2 | spl4_71)), inference(forward_demodulation, [], [f601, f236])).
fof(f601, plain, (~ (e0 = op(op(e3, e3), op(e3, e3))) | spl4_71), inference(avatar_component_clause, [], [f599])).
fof(f2405, plain, (~ spl4_60 | ~ spl4_12), inference(avatar_split_clause, [], [f2229, f276, f480])).
fof(f480, plain, (spl4_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_60])])).
fof(f276, plain, (spl4_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_12])])).
fof(f2229, plain, (~ (e3 = op(e0, e1)) | ~ spl4_12), inference(forward_demodulation, [], [f119, f278])).
fof(f278, plain, ((e3 = op(e3, e1)) | ~ spl4_12), inference(avatar_component_clause, [], [f276])).
fof(f119, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2399, plain, (~ spl4_43 | ~ spl4_2 | spl4_87), inference(avatar_split_clause, [], [f2362, f675, f234, f408])).
fof(f675, plain, (spl4_87 <=> (e2 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_87])])).
fof(f2362, plain, (~ (e2 = op(e1, e1)) | (~ spl4_2 | spl4_87)), inference(backward_demodulation, [], [f677, f236])).
fof(f677, plain, (~ (e2 = op(op(e3, e3), op(e3, e3))) | spl4_87), inference(avatar_component_clause, [], [f675])).
fof(f2390, plain, (~ spl4_2 | ~ spl4_41 | ~ spl4_59 | spl4_75), inference(avatar_contradiction_clause, [], [f2389])).
fof(f2389, plain, ($false | (~ spl4_2 | ~ spl4_41 | ~ spl4_59 | spl4_75)), inference(subsumption_resolution, [], [f2388, f478])).
fof(f2388, plain, (~ (e2 = op(e0, e1)) | (~ spl4_2 | ~ spl4_41 | spl4_75)), inference(forward_demodulation, [], [f2387, f402])).
fof(f2387, plain, (~ (e2 = op(op(e1, e1), e1)) | (~ spl4_2 | spl4_75)), inference(forward_demodulation, [], [f619, f236])).
fof(f619, plain, (~ (e2 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | spl4_75), inference(avatar_component_clause, [], [f617])).
fof(f617, plain, (spl4_75 <=> (e2 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_75])])).
fof(f2350, plain, (~ spl4_5 | ~ spl4_21), inference(avatar_split_clause, [], [f2342, f315, f247])).
fof(f247, plain, (spl4_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_5])])).
fof(f2342, plain, (~ (e0 = op(e3, e2)) | ~ spl4_21), inference(backward_demodulation, [], [f128, f317])).
fof(f317, plain, ((e0 = op(e2, e2)) | ~ spl4_21), inference(avatar_component_clause, [], [f315])).
fof(f128, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2311, plain, (~ spl4_24 | ~ spl4_56), inference(avatar_split_clause, [], [f2310, f463, f327])).
fof(f463, plain, (spl4_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_56])])).
fof(f2310, plain, (~ (e3 = op(e2, e2)) | ~ spl4_56), inference(backward_demodulation, [], [f124, f465])).
fof(f465, plain, ((e3 = op(e0, e2)) | ~ spl4_56), inference(avatar_component_clause, [], [f463])).
fof(f2304, plain, (~ spl4_43 | ~ spl4_62 | spl4_88), inference(avatar_split_clause, [], [f2303, f680, f489, f408])).
fof(f489, plain, (spl4_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl4_62])])).
fof(f680, plain, (spl4_88 <=> (e2 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_88])])).
fof(f2303, plain, (~ (e2 = op(e1, e1)) | (~ spl4_62 | spl4_88)), inference(forward_demodulation, [], [f682, f491])).
fof(f491, plain, ((op(e0, e0) = e1) | ~ spl4_62), inference(avatar_component_clause, [], [f489])).
fof(f682, plain, (~ (e2 = op(op(e0, e0), op(e0, e0))) | spl4_88), inference(avatar_component_clause, [], [f680])).
fof(f2292, plain, (~ spl4_27 | ~ spl4_59), inference(avatar_split_clause, [], [f2291, f476, f340])).
fof(f340, plain, (spl4_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_27])])).
fof(f2291, plain, (~ (e2 = op(e2, e1)) | ~ spl4_59), inference(forward_demodulation, [], [f118, f478])).
fof(f2281, plain, (~ spl4_10 | ~ spl4_12), inference(avatar_contradiction_clause, [], [f2280])).
fof(f2280, plain, ($false | (~ spl4_10 | ~ spl4_12)), inference(subsumption_resolution, [], [f2278, f163])).
fof(f163, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax4)).
fof(f2278, plain, ((e1 = e3) | (~ spl4_10 | ~ spl4_12)), inference(backward_demodulation, [], [f278, f270])).
fof(f270, plain, ((e1 = op(e3, e1)) | ~ spl4_10), inference(avatar_component_clause, [], [f268])).
fof(f268, plain, (spl4_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_10])])).
fof(f2260, plain, (~ spl4_44 | ~ spl4_48), inference(avatar_split_clause, [], [f2257, f429, f412])).
fof(f412, plain, (spl4_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_44])])).
fof(f429, plain, (spl4_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_48])])).
fof(f2257, plain, (~ (e3 = op(e1, e1)) | ~ spl4_48), inference(backward_demodulation, [], [f141, f431])).
fof(f431, plain, ((e3 = op(e1, e0)) | ~ spl4_48), inference(avatar_component_clause, [], [f429])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2250, plain, (~ spl4_14 | ~ spl4_62), inference(avatar_split_clause, [], [f2243, f489, f285])).
fof(f2243, plain, (~ (e1 = op(e3, e0)) | ~ spl4_62), inference(backward_demodulation, [], [f113, f491])).
fof(f113, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2218, plain, (~ spl4_44 | ~ spl4_12), inference(avatar_split_clause, [], [f2217, f276, f412])).
fof(f2217, plain, (~ (e3 = op(e1, e1)) | ~ spl4_12), inference(forward_demodulation, [], [f121, f278])).
fof(f121, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2210, plain, (~ spl4_33 | ~ spl4_49), inference(avatar_split_clause, [], [f2168, f434, f366])).
fof(f2168, plain, (~ (e0 = op(e1, e3)) | ~ spl4_49), inference(backward_demodulation, [], [f129, f436])).
fof(f436, plain, ((e0 = op(e0, e3)) | ~ spl4_49), inference(avatar_component_clause, [], [f434])).
fof(f129, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2186, plain, (~ spl4_27 | ~ spl4_31), inference(avatar_split_clause, [], [f2184, f357, f340])).
fof(f2184, plain, (~ (e2 = op(e2, e1)) | ~ spl4_31), inference(backward_demodulation, [], [f147, f359])).
fof(f359, plain, ((e2 = op(e2, e0)) | ~ spl4_31), inference(avatar_component_clause, [], [f357])).
fof(f2179, plain, (~ spl4_34 | ~ spl4_38), inference(avatar_split_clause, [], [f2175, f387, f370])).
fof(f370, plain, (spl4_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_34])])).
fof(f2175, plain, (~ (e1 = op(e1, e3)) | ~ spl4_38), inference(backward_demodulation, [], [f146, f389])).
fof(f389, plain, ((e1 = op(e1, e2)) | ~ spl4_38), inference(avatar_component_clause, [], [f387])).
fof(f146, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2178, plain, (~ spl4_6 | ~ spl4_38), inference(avatar_split_clause, [], [f2174, f387, f251])).
fof(f251, plain, (spl4_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_6])])).
fof(f2174, plain, (~ (e1 = op(e3, e2)) | ~ spl4_38), inference(backward_demodulation, [], [f127, f389])).
fof(f127, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2173, plain, (~ spl4_17 | ~ spl4_49), inference(avatar_split_clause, [], [f2169, f434, f298])).
fof(f298, plain, (spl4_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_17])])).
fof(f2169, plain, (~ (e0 = op(e2, e3)) | ~ spl4_49), inference(backward_demodulation, [], [f130, f436])).
fof(f2158, plain, (~ spl4_18 | ~ spl4_3 | ~ spl4_64 | spl4_89), inference(avatar_split_clause, [], [f2157, f684, f497, f238, f302])).
fof(f302, plain, (spl4_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_18])])).
fof(f684, plain, (spl4_89 <=> (e1 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_89])])).
fof(f2157, plain, (~ (e1 = op(e2, e3)) | (~ spl4_3 | ~ spl4_64 | spl4_89)), inference(forward_demodulation, [], [f2150, f240])).
fof(f240, plain, ((e2 = op(e3, e3)) | ~ spl4_3), inference(avatar_component_clause, [], [f238])).
fof(f2150, plain, (~ (e1 = op(op(e3, e3), e3)) | (~ spl4_64 | spl4_89)), inference(backward_demodulation, [], [f686, f499])).
fof(f686, plain, (~ (e1 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | spl4_89), inference(avatar_component_clause, [], [f684])).
fof(f2156, plain, (~ spl4_3 | ~ spl4_64 | spl4_88), inference(avatar_contradiction_clause, [], [f2155])).
fof(f2155, plain, ($false | (~ spl4_3 | ~ spl4_64 | spl4_88)), inference(subsumption_resolution, [], [f2149, f240])).
fof(f2149, plain, (~ (e2 = op(e3, e3)) | (~ spl4_64 | spl4_88)), inference(backward_demodulation, [], [f682, f499])).
fof(f2151, plain, (~ spl4_16 | ~ spl4_64), inference(avatar_split_clause, [], [f2143, f497, f293])).
fof(f293, plain, (spl4_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_16])])).
fof(f2143, plain, (~ (e3 = op(e3, e0)) | ~ spl4_64), inference(backward_demodulation, [], [f113, f499])).
fof(f2126, plain, (~ spl4_40 | ~ spl4_24), inference(avatar_split_clause, [], [f2125, f327, f395])).
fof(f395, plain, (spl4_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_40])])).
fof(f2125, plain, (~ (e3 = op(e1, e2)) | ~ spl4_24), inference(forward_demodulation, [], [f126, f329])).
fof(f126, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2124, plain, (~ spl4_40 | ~ spl4_44), inference(avatar_split_clause, [], [f2123, f412, f395])).
fof(f2123, plain, (~ (e3 = op(e1, e2)) | ~ spl4_44), inference(forward_demodulation, [], [f144, f414])).
fof(f414, plain, ((e3 = op(e1, e1)) | ~ spl4_44), inference(avatar_component_clause, [], [f412])).
fof(f144, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2067, plain, (~ spl4_39 | ~ spl4_40), inference(avatar_contradiction_clause, [], [f2066])).
fof(f2066, plain, ($false | (~ spl4_39 | ~ spl4_40)), inference(subsumption_resolution, [], [f2065, f164])).
fof(f164, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f2065, plain, ((e2 = e3) | (~ spl4_39 | ~ spl4_40)), inference(backward_demodulation, [], [f397, f393])).
fof(f393, plain, ((e2 = op(e1, e2)) | ~ spl4_39), inference(avatar_component_clause, [], [f391])).
fof(f391, plain, (spl4_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_39])])).
fof(f397, plain, ((e3 = op(e1, e2)) | ~ spl4_40), inference(avatar_component_clause, [], [f395])).
fof(f2063, plain, (~ spl4_17 | ~ spl4_44 | ~ spl4_85 | spl4_86), inference(avatar_split_clause, [], [f2059, f670, f666, f412, f298])).
fof(f666, plain, (spl4_85 <=> (e2 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl4_85])])).
fof(f670, plain, (spl4_86 <=> (e0 = op(op(op(e1, e1), op(e1, e1)), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl4_86])])).
fof(f2059, plain, (~ (e0 = op(e2, e3)) | (~ spl4_44 | ~ spl4_85 | spl4_86)), inference(backward_demodulation, [], [f2024, f414])).
fof(f2024, plain, (~ (e0 = op(e2, op(e1, e1))) | (~ spl4_85 | spl4_86)), inference(backward_demodulation, [], [f672, f667])).
fof(f667, plain, ((e2 = op(op(e1, e1), op(e1, e1))) | ~ spl4_85), inference(avatar_component_clause, [], [f666])).
fof(f672, plain, (~ (e0 = op(op(op(e1, e1), op(e1, e1)), op(e1, e1))) | spl4_86), inference(avatar_component_clause, [], [f670])).
fof(f2054, plain, (~ spl4_29 | ~ spl4_45), inference(avatar_split_clause, [], [f2049, f417, f349])).
fof(f417, plain, (spl4_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_45])])).
fof(f2049, plain, (~ (e0 = op(e2, e0)) | ~ spl4_45), inference(backward_demodulation, [], [f114, f419])).
fof(f419, plain, ((e0 = op(e1, e0)) | ~ spl4_45), inference(avatar_component_clause, [], [f417])).
fof(f114, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2039, plain, (~ spl4_22 | ~ spl4_63 | spl4_82), inference(avatar_split_clause, [], [f2032, f650, f493, f319])).
fof(f2032, plain, (~ (e1 = op(e2, e2)) | (~ spl4_63 | spl4_82)), inference(backward_demodulation, [], [f652, f495])).
fof(f2038, plain, (~ spl4_51 | ~ spl4_63), inference(avatar_split_clause, [], [f2030, f493, f442])).
fof(f442, plain, (spl4_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_51])])).
fof(f2030, plain, (~ (e2 = op(e0, e3)) | ~ spl4_63), inference(backward_demodulation, [], [f137, f495])).
fof(f2037, plain, (~ spl4_47 | ~ spl4_63), inference(avatar_split_clause, [], [f2027, f493, f425])).
fof(f425, plain, (spl4_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_47])])).
fof(f2027, plain, (~ (e2 = op(e1, e0)) | ~ spl4_63), inference(backward_demodulation, [], [f111, f495])).
fof(f111, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f1989, plain, (~ spl4_8 | ~ spl4_16), inference(avatar_split_clause, [], [f1987, f293, f259])).
fof(f259, plain, (spl4_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_8])])).
fof(f1987, plain, (~ (e3 = op(e3, e2)) | ~ spl4_16), inference(backward_demodulation, [], [f154, f295])).
fof(f295, plain, ((e3 = op(e3, e0)) | ~ spl4_16), inference(avatar_component_clause, [], [f293])).
fof(f154, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1985, plain, (~ spl4_2 | ~ spl4_34), inference(avatar_split_clause, [], [f1982, f370, f234])).
fof(f1982, plain, (~ (e1 = op(e3, e3)) | ~ spl4_34), inference(backward_demodulation, [], [f133, f372])).
fof(f372, plain, ((e1 = op(e1, e3)) | ~ spl4_34), inference(avatar_component_clause, [], [f370])).
fof(f133, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1978, plain, (~ spl4_22 | ~ spl4_41 | ~ spl4_60 | spl4_77), inference(avatar_contradiction_clause, [], [f1977])).
fof(f1977, plain, ($false | (~ spl4_22 | ~ spl4_41 | ~ spl4_60 | spl4_77)), inference(subsumption_resolution, [], [f1966, f482])).
fof(f482, plain, ((e3 = op(e0, e1)) | ~ spl4_60), inference(avatar_component_clause, [], [f480])).
fof(f1966, plain, (~ (e3 = op(e0, e1)) | (~ spl4_22 | ~ spl4_41 | spl4_77)), inference(backward_demodulation, [], [f1914, f402])).
fof(f1914, plain, (~ (e3 = op(op(e1, e1), e1)) | (~ spl4_22 | spl4_77)), inference(forward_demodulation, [], [f629, f321])).
fof(f321, plain, ((e1 = op(e2, e2)) | ~ spl4_22), inference(avatar_component_clause, [], [f319])).
fof(f629, plain, (~ (e3 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) | spl4_77), inference(avatar_component_clause, [], [f627])).
fof(f627, plain, (spl4_77 <=> (e3 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_77])])).
fof(f1940, plain, (~ spl4_37 | ~ spl4_53), inference(avatar_split_clause, [], [f1934, f451, f383])).
fof(f383, plain, (spl4_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_37])])).
fof(f1934, plain, (~ (e0 = op(e1, e2)) | ~ spl4_53), inference(backward_demodulation, [], [f123, f453])).
fof(f123, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1928, plain, (~ spl4_58 | ~ spl4_62), inference(avatar_split_clause, [], [f1917, f489, f472])).
fof(f1917, plain, (~ (e1 = op(e0, e1)) | ~ spl4_62), inference(backward_demodulation, [], [f135, f491])).
fof(f1927, plain, (~ spl4_46 | ~ spl4_62), inference(avatar_split_clause, [], [f1915, f489, f421])).
fof(f1915, plain, (~ (e1 = op(e1, e0)) | ~ spl4_62), inference(backward_demodulation, [], [f111, f491])).
fof(f1903, plain, (~ spl4_41 | ~ spl4_22 | spl4_69), inference(avatar_split_clause, [], [f1882, f590, f319, f400])).
fof(f1882, plain, (~ (e0 = op(e1, e1)) | (~ spl4_22 | spl4_69)), inference(backward_demodulation, [], [f592, f321])).
fof(f1902, plain, (~ spl4_38 | ~ spl4_22), inference(avatar_split_clause, [], [f1901, f319, f387])).
fof(f1901, plain, (~ (e1 = op(e1, e2)) | ~ spl4_22), inference(forward_demodulation, [], [f126, f321])).
fof(f1900, plain, (~ spl4_36 | ~ spl4_20), inference(avatar_split_clause, [], [f1899, f310, f378])).
fof(f378, plain, (spl4_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_36])])).
fof(f1899, plain, (~ (e3 = op(e1, e3)) | ~ spl4_20), inference(forward_demodulation, [], [f132, f312])).
fof(f312, plain, ((e3 = op(e2, e3)) | ~ spl4_20), inference(avatar_component_clause, [], [f310])).
fof(f132, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1877, plain, (~ spl4_19 | ~ spl4_27), inference(avatar_split_clause, [], [f1874, f340, f306])).
fof(f1874, plain, (~ (e2 = op(e2, e3)) | ~ spl4_27), inference(backward_demodulation, [], [f151, f342])).
fof(f342, plain, ((e2 = op(e2, e1)) | ~ spl4_27), inference(avatar_component_clause, [], [f340])).
fof(f151, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1876, plain, (~ spl4_11 | ~ spl4_27), inference(avatar_split_clause, [], [f1872, f340, f272])).
fof(f272, plain, (spl4_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_11])])).
fof(f1872, plain, (~ (e2 = op(e3, e1)) | ~ spl4_27), inference(backward_demodulation, [], [f122, f342])).
fof(f122, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1871, plain, (~ spl4_21 | ~ spl4_29), inference(avatar_split_clause, [], [f1866, f349, f315])).
fof(f1866, plain, (~ (e0 = op(e2, e2)) | ~ spl4_29), inference(backward_demodulation, [], [f148, f351])).
fof(f148, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1834, plain, (~ spl4_30 | ~ spl4_46), inference(avatar_split_clause, [], [f1830, f421, f353])).
fof(f353, plain, (spl4_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_30])])).
fof(f1830, plain, (~ (e1 = op(e2, e0)) | ~ spl4_46), inference(backward_demodulation, [], [f114, f423])).
fof(f1822, plain, (~ spl4_35 | ~ spl4_64 | ~ spl4_82 | spl4_83), inference(avatar_contradiction_clause, [], [f1821])).
fof(f1821, plain, ($false | (~ spl4_35 | ~ spl4_64 | ~ spl4_82 | spl4_83)), inference(subsumption_resolution, [], [f1815, f376])).
fof(f376, plain, ((e2 = op(e1, e3)) | ~ spl4_35), inference(avatar_component_clause, [], [f374])).
fof(f374, plain, (spl4_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_35])])).
fof(f1815, plain, (~ (e2 = op(e1, e3)) | (~ spl4_64 | ~ spl4_82 | spl4_83)), inference(backward_demodulation, [], [f1807, f499])).
fof(f1807, plain, (~ (e2 = op(e1, op(e0, e0))) | (~ spl4_82 | spl4_83)), inference(backward_demodulation, [], [f656, f651])).
fof(f656, plain, (~ (e2 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | spl4_83), inference(avatar_component_clause, [], [f654])).
fof(f654, plain, (spl4_83 <=> (e2 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_83])])).
fof(f1817, plain, (~ spl4_48 | ~ spl4_64), inference(avatar_split_clause, [], [f1809, f497, f429])).
fof(f1809, plain, (~ (e3 = op(e1, e0)) | ~ spl4_64), inference(backward_demodulation, [], [f111, f499])).
fof(f1804, plain, (~ spl4_63 | ~ spl4_55), inference(avatar_split_clause, [], [f1803, f459, f493])).
fof(f459, plain, (spl4_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_55])])).
fof(f1803, plain, (~ (op(e0, e0) = e2) | ~ spl4_55), inference(forward_demodulation, [], [f136, f461])).
fof(f461, plain, ((e2 = op(e0, e2)) | ~ spl4_55), inference(avatar_component_clause, [], [f459])).
fof(f136, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f1762, plain, (~ spl4_14 | ~ spl4_21 | spl4_70 | ~ spl4_91), inference(avatar_split_clause, [], [f1754, f697, f594, f315, f285])).
fof(f697, plain, (spl4_91 <=> (e3 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_91])])).
fof(f1754, plain, (~ (e1 = op(e3, e0)) | (~ spl4_21 | spl4_70 | ~ spl4_91)), inference(backward_demodulation, [], [f1564, f317])).
fof(f1564, plain, (~ (e1 = op(e3, op(e2, e2))) | (spl4_70 | ~ spl4_91)), inference(forward_demodulation, [], [f596, f698])).
fof(f698, plain, ((e3 = op(op(e2, e2), op(e2, e2))) | ~ spl4_91), inference(avatar_component_clause, [], [f697])).
fof(f1730, plain, (~ spl4_45 | ~ spl4_48), inference(avatar_contradiction_clause, [], [f1729])).
fof(f1729, plain, ($false | (~ spl4_45 | ~ spl4_48)), inference(subsumption_resolution, [], [f1727, f161])).
fof(f161, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f1727, plain, ((e0 = e3) | (~ spl4_45 | ~ spl4_48)), inference(backward_demodulation, [], [f431, f419])).
fof(f1709, plain, (~ spl4_30 | ~ spl4_1 | ~ spl4_63 | spl4_72), inference(avatar_split_clause, [], [f1702, f603, f493, f230, f353])).
fof(f603, plain, (spl4_72 <=> (e1 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_72])])).
fof(f1702, plain, (~ (e1 = op(e2, e0)) | (~ spl4_1 | ~ spl4_63 | spl4_72)), inference(backward_demodulation, [], [f1689, f495])).
fof(f1689, plain, (~ (e1 = op(op(e0, e0), e0)) | (~ spl4_1 | spl4_72)), inference(forward_demodulation, [], [f605, f232])).
fof(f605, plain, (~ (e1 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | spl4_72), inference(avatar_component_clause, [], [f603])).
fof(f1680, plain, (~ spl4_54 | ~ spl4_38), inference(avatar_split_clause, [], [f1679, f387, f455])).
fof(f455, plain, (spl4_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_54])])).
fof(f1679, plain, (~ (e1 = op(e0, e2)) | ~ spl4_38), inference(forward_demodulation, [], [f123, f389])).
fof(f1677, plain, (~ spl4_51 | ~ spl4_19), inference(avatar_split_clause, [], [f1676, f306, f442])).
fof(f1676, plain, (~ (e2 = op(e0, e3)) | ~ spl4_19), inference(forward_demodulation, [], [f130, f308])).
fof(f1674, plain, (~ spl4_43 | ~ spl4_11), inference(avatar_split_clause, [], [f1561, f272, f408])).
fof(f1561, plain, (~ (e2 = op(e1, e1)) | ~ spl4_11), inference(forward_demodulation, [], [f121, f274])).
fof(f274, plain, ((e2 = op(e3, e1)) | ~ spl4_11), inference(avatar_component_clause, [], [f272])).
fof(f1672, plain, (~ spl4_42 | ~ spl4_38), inference(avatar_split_clause, [], [f1671, f387, f404])).
fof(f404, plain, (spl4_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_42])])).
fof(f1671, plain, (~ (e1 = op(e1, e1)) | ~ spl4_38), inference(forward_demodulation, [], [f144, f389])).
fof(f1663, plain, (~ spl4_31 | ~ spl4_19), inference(avatar_split_clause, [], [f1662, f306, f357])).
fof(f1662, plain, (~ (e2 = op(e2, e0)) | ~ spl4_19), inference(forward_demodulation, [], [f149, f308])).
fof(f149, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1661, plain, (~ spl4_23 | ~ spl4_19), inference(avatar_split_clause, [], [f1660, f306, f323])).
fof(f323, plain, (spl4_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_23])])).
fof(f1660, plain, (~ (e2 = op(e2, e2)) | ~ spl4_19), inference(forward_demodulation, [], [f152, f308])).
fof(f152, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1655, plain, (~ spl4_32 | ~ spl4_16), inference(avatar_split_clause, [], [f1654, f293, f361])).
fof(f1654, plain, (~ (e3 = op(e2, e0)) | ~ spl4_16), inference(forward_demodulation, [], [f116, f295])).
fof(f1653, plain, (~ spl4_48 | ~ spl4_16), inference(avatar_split_clause, [], [f1652, f293, f429])).
fof(f1652, plain, (~ (e3 = op(e1, e0)) | ~ spl4_16), inference(forward_demodulation, [], [f115, f295])).
fof(f1614, plain, (~ spl4_37 | ~ spl4_38), inference(avatar_contradiction_clause, [], [f1613])).
fof(f1613, plain, ($false | (~ spl4_37 | ~ spl4_38)), inference(subsumption_resolution, [], [f1612, f159])).
fof(f159, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1612, plain, ((e0 = e1) | (~ spl4_37 | ~ spl4_38)), inference(backward_demodulation, [], [f389, f385])).
fof(f385, plain, ((e0 = op(e1, e2)) | ~ spl4_37), inference(avatar_component_clause, [], [f383])).
fof(f1611, plain, (~ spl4_38 | ~ spl4_40), inference(avatar_contradiction_clause, [], [f1610])).
fof(f1610, plain, ($false | (~ spl4_38 | ~ spl4_40)), inference(subsumption_resolution, [], [f1609, f163])).
fof(f1609, plain, ((e1 = e3) | (~ spl4_38 | ~ spl4_40)), inference(backward_demodulation, [], [f397, f389])).
fof(f1599, plain, (~ spl4_34 | ~ spl4_42), inference(avatar_split_clause, [], [f1592, f404, f370])).
fof(f1592, plain, (~ (e1 = op(e1, e3)) | ~ spl4_42), inference(backward_demodulation, [], [f145, f406])).
fof(f406, plain, ((e1 = op(e1, e1)) | ~ spl4_42), inference(avatar_component_clause, [], [f404])).
fof(f1589, plain, (~ spl4_31 | ~ spl4_47), inference(avatar_split_clause, [], [f1587, f425, f357])).
fof(f1587, plain, (~ (e2 = op(e2, e0)) | ~ spl4_47), inference(backward_demodulation, [], [f114, f427])).
fof(f427, plain, ((e2 = op(e1, e0)) | ~ spl4_47), inference(avatar_component_clause, [], [f425])).
fof(f1588, plain, (~ spl4_39 | ~ spl4_47), inference(avatar_split_clause, [], [f1585, f425, f391])).
fof(f1585, plain, (~ (e2 = op(e1, e2)) | ~ spl4_47), inference(backward_demodulation, [], [f142, f427])).
fof(f1503, plain, (~ spl4_10 | ~ spl4_26), inference(avatar_split_clause, [], [f1499, f336, f268])).
fof(f1499, plain, (~ (e1 = op(e3, e1)) | ~ spl4_26), inference(backward_demodulation, [], [f122, f338])).
fof(f1489, plain, (~ spl4_13 | ~ spl4_45), inference(avatar_split_clause, [], [f1486, f417, f281])).
fof(f1486, plain, (~ (e0 = op(e3, e0)) | ~ spl4_45), inference(backward_demodulation, [], [f115, f419])).
fof(f1479, plain, (~ spl4_22 | ~ spl4_54), inference(avatar_split_clause, [], [f1478, f455, f319])).
fof(f1478, plain, (~ (e1 = op(e2, e2)) | ~ spl4_54), inference(backward_demodulation, [], [f124, f457])).
fof(f457, plain, ((e1 = op(e0, e2)) | ~ spl4_54), inference(avatar_component_clause, [], [f455])).
fof(f1408, plain, (~ spl4_57 | ~ spl4_59), inference(avatar_contradiction_clause, [], [f1407])).
fof(f1407, plain, ($false | (~ spl4_57 | ~ spl4_59)), inference(subsumption_resolution, [], [f1406, f160])).
fof(f160, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f1406, plain, ((e0 = e2) | (~ spl4_57 | ~ spl4_59)), inference(forward_demodulation, [], [f478, f470])).
fof(f470, plain, ((e0 = op(e0, e1)) | ~ spl4_57), inference(avatar_component_clause, [], [f468])).
fof(f468, plain, (spl4_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_57])])).
fof(f1384, plain, (~ spl4_28 | ~ spl4_43 | ~ spl4_62 | spl4_84), inference(avatar_contradiction_clause, [], [f1383])).
fof(f1383, plain, ($false | (~ spl4_28 | ~ spl4_43 | ~ spl4_62 | spl4_84)), inference(subsumption_resolution, [], [f1382, f346])).
fof(f346, plain, ((e3 = op(e2, e1)) | ~ spl4_28), inference(avatar_component_clause, [], [f344])).
fof(f344, plain, (spl4_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_28])])).
fof(f1382, plain, (~ (e3 = op(e2, e1)) | (~ spl4_43 | ~ spl4_62 | spl4_84)), inference(forward_demodulation, [], [f1381, f410])).
fof(f1381, plain, (~ (e3 = op(op(e1, e1), e1)) | (~ spl4_62 | spl4_84)), inference(forward_demodulation, [], [f662, f491])).
fof(f662, plain, (~ (e3 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | spl4_84), inference(avatar_component_clause, [], [f660])).
fof(f660, plain, (spl4_84 <=> (e3 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_84])])).
fof(f1326, plain, (~ spl4_11 | ~ spl4_62 | spl4_83 | ~ spl4_92), inference(avatar_split_clause, [], [f1322, f702, f654, f489, f272])).
fof(f702, plain, (spl4_92 <=> (e3 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_92])])).
fof(f1322, plain, (~ (e2 = op(e3, e1)) | (~ spl4_62 | spl4_83 | ~ spl4_92)), inference(backward_demodulation, [], [f1311, f491])).
fof(f1311, plain, (~ (e2 = op(e3, op(e0, e0))) | (spl4_83 | ~ spl4_92)), inference(forward_demodulation, [], [f656, f703])).
fof(f703, plain, ((e3 = op(op(e0, e0), op(e0, e0))) | ~ spl4_92), inference(avatar_component_clause, [], [f702])).
fof(f1276, plain, (~ spl4_3 | ~ spl4_44 | spl4_85), inference(avatar_split_clause, [], [f1236, f666, f412, f238])).
fof(f1236, plain, (~ (e2 = op(e3, e3)) | (~ spl4_44 | spl4_85)), inference(backward_demodulation, [], [f668, f414])).
fof(f668, plain, (~ (e2 = op(op(e1, e1), op(e1, e1))) | spl4_85), inference(avatar_component_clause, [], [f666])).
fof(f1267, plain, (~ spl4_17 | ~ spl4_20), inference(avatar_contradiction_clause, [], [f1266])).
fof(f1266, plain, ($false | (~ spl4_17 | ~ spl4_20)), inference(subsumption_resolution, [], [f1265, f161])).
fof(f1265, plain, ((e0 = e3) | (~ spl4_17 | ~ spl4_20)), inference(forward_demodulation, [], [f312, f300])).
fof(f300, plain, ((e0 = op(e2, e3)) | ~ spl4_17), inference(avatar_component_clause, [], [f298])).
fof(f1250, plain, (~ spl4_15 | ~ spl4_31), inference(avatar_split_clause, [], [f1249, f357, f289])).
fof(f289, plain, (spl4_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_15])])).
fof(f1249, plain, (~ (e2 = op(e3, e0)) | ~ spl4_31), inference(backward_demodulation, [], [f116, f359])).
fof(f1232, plain, (~ spl4_43 | ~ spl4_47), inference(avatar_split_clause, [], [f1229, f425, f408])).
fof(f1229, plain, (~ (e2 = op(e1, e1)) | ~ spl4_47), inference(backward_demodulation, [], [f141, f427])).
fof(f1223, plain, (~ spl4_49 | ~ spl4_57), inference(avatar_split_clause, [], [f1219, f468, f434])).
fof(f1219, plain, (~ (e0 = op(e0, e3)) | ~ spl4_57), inference(backward_demodulation, [], [f139, f470])).
fof(f139, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1222, plain, (~ spl4_9 | ~ spl4_57), inference(avatar_split_clause, [], [f1217, f468, f264])).
fof(f264, plain, (spl4_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_9])])).
fof(f1217, plain, (~ (e0 = op(e3, e1)) | ~ spl4_57), inference(backward_demodulation, [], [f119, f470])).
fof(f1211, plain, (~ spl4_60 | ~ spl4_64), inference(avatar_split_clause, [], [f1204, f497, f480])).
fof(f1204, plain, (~ (e3 = op(e0, e1)) | ~ spl4_64), inference(backward_demodulation, [], [f135, f499])).
fof(f1178, plain, (~ spl4_24 | ~ spl4_8), inference(avatar_split_clause, [], [f1177, f259, f327])).
fof(f1177, plain, (~ (e3 = op(e2, e2)) | ~ spl4_8), inference(forward_demodulation, [], [f128, f261])).
fof(f261, plain, ((e3 = op(e3, e2)) | ~ spl4_8), inference(avatar_component_clause, [], [f259])).
fof(f1159, plain, (~ spl4_3 | ~ spl4_54 | ~ spl4_71 | spl4_72), inference(avatar_contradiction_clause, [], [f1158])).
fof(f1158, plain, ($false | (~ spl4_3 | ~ spl4_54 | ~ spl4_71 | spl4_72)), inference(subsumption_resolution, [], [f1154, f457])).
fof(f1154, plain, (~ (e1 = op(e0, e2)) | (~ spl4_3 | ~ spl4_71 | spl4_72)), inference(backward_demodulation, [], [f1117, f240])).
fof(f1117, plain, (~ (e1 = op(e0, op(e3, e3))) | (~ spl4_71 | spl4_72)), inference(backward_demodulation, [], [f605, f600])).
fof(f600, plain, ((e0 = op(op(e3, e3), op(e3, e3))) | ~ spl4_71), inference(avatar_component_clause, [], [f599])).
fof(f1116, plain, (~ spl4_64 | ~ spl4_21 | spl4_91), inference(avatar_split_clause, [], [f1115, f697, f315, f497])).
fof(f1115, plain, (~ (op(e0, e0) = e3) | (~ spl4_21 | spl4_91)), inference(forward_demodulation, [], [f699, f317])).
fof(f699, plain, (~ (e3 = op(op(e2, e2), op(e2, e2))) | spl4_91), inference(avatar_component_clause, [], [f697])).
fof(f1114, plain, (~ spl4_63 | ~ spl4_15), inference(avatar_split_clause, [], [f1113, f289, f493])).
fof(f1113, plain, (~ (op(e0, e0) = e2) | ~ spl4_15), inference(forward_demodulation, [], [f113, f291])).
fof(f291, plain, ((e2 = op(e3, e0)) | ~ spl4_15), inference(avatar_component_clause, [], [f289])).
fof(f1102, plain, (~ spl4_1 | ~ spl4_49), inference(avatar_split_clause, [], [f1101, f434, f230])).
fof(f1101, plain, (~ (e0 = op(e3, e3)) | ~ spl4_49), inference(forward_demodulation, [], [f131, f436])).
fof(f131, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1092, plain, (~ spl4_42 | ~ spl4_46), inference(avatar_split_clause, [], [f1091, f421, f404])).
fof(f1091, plain, (~ (e1 = op(e1, e1)) | ~ spl4_46), inference(forward_demodulation, [], [f141, f423])).
fof(f1087, plain, (~ spl4_43 | ~ spl4_27), inference(avatar_split_clause, [], [f1015, f340, f408])).
fof(f1015, plain, (~ (e2 = op(e1, e1)) | ~ spl4_27), inference(forward_demodulation, [], [f120, f342])).
fof(f1079, plain, (~ spl4_3 | ~ spl4_15), inference(avatar_split_clause, [], [f1078, f289, f238])).
fof(f1078, plain, (~ (e2 = op(e3, e3)) | ~ spl4_15), inference(backward_demodulation, [], [f155, f291])).
fof(f155, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1053, plain, (~ spl4_49 | ~ spl4_51), inference(avatar_contradiction_clause, [], [f1052])).
fof(f1052, plain, ($false | (~ spl4_49 | ~ spl4_51)), inference(subsumption_resolution, [], [f1051, f160])).
fof(f1051, plain, ((e0 = e2) | (~ spl4_49 | ~ spl4_51)), inference(backward_demodulation, [], [f444, f436])).
fof(f444, plain, ((e2 = op(e0, e3)) | ~ spl4_51), inference(avatar_component_clause, [], [f442])).
fof(f1041, plain, (~ spl4_44 | ~ spl4_62 | spl4_92), inference(avatar_contradiction_clause, [], [f1040])).
fof(f1040, plain, ($false | (~ spl4_44 | ~ spl4_62 | spl4_92)), inference(subsumption_resolution, [], [f1033, f414])).
fof(f1033, plain, (~ (e3 = op(e1, e1)) | (~ spl4_62 | spl4_92)), inference(backward_demodulation, [], [f704, f491])).
fof(f704, plain, (~ (e3 = op(op(e0, e0), op(e0, e0))) | spl4_92), inference(avatar_component_clause, [], [f702])).
fof(f1019, plain, (~ spl4_63 | ~ spl4_1 | spl4_87), inference(avatar_split_clause, [], [f969, f675, f230, f493])).
fof(f969, plain, (~ (op(e0, e0) = e2) | (~ spl4_1 | spl4_87)), inference(backward_demodulation, [], [f677, f232])).
fof(f997, plain, (~ spl4_34 | ~ spl4_46), inference(avatar_split_clause, [], [f996, f421, f370])).
fof(f996, plain, (~ (e1 = op(e1, e3)) | ~ spl4_46), inference(forward_demodulation, [], [f143, f423])).
fof(f143, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f986, plain, (~ spl4_12 | ~ spl4_8), inference(avatar_split_clause, [], [f984, f259, f276])).
fof(f984, plain, (~ (e3 = op(e3, e1)) | ~ spl4_8), inference(backward_demodulation, [], [f156, f261])).
fof(f156, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f981, plain, (~ spl4_8 | ~ spl4_56), inference(avatar_split_clause, [], [f980, f463, f259])).
fof(f980, plain, (~ (e3 = op(e3, e2)) | ~ spl4_56), inference(forward_demodulation, [], [f125, f465])).
fof(f125, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f950, plain, (~ spl4_31 | ~ spl4_32), inference(avatar_contradiction_clause, [], [f949])).
fof(f949, plain, ($false | (~ spl4_31 | ~ spl4_32)), inference(subsumption_resolution, [], [f948, f164])).
fof(f948, plain, ((e2 = e3) | (~ spl4_31 | ~ spl4_32)), inference(backward_demodulation, [], [f363, f359])).
fof(f363, plain, ((e3 = op(e2, e0)) | ~ spl4_32), inference(avatar_component_clause, [], [f361])).
fof(f924, plain, (~ spl4_57 | ~ spl4_41), inference(avatar_split_clause, [], [f923, f400, f468])).
fof(f923, plain, (~ (e0 = op(e0, e1)) | ~ spl4_41), inference(forward_demodulation, [], [f117, f402])).
fof(f914, plain, (~ spl4_50 | ~ spl4_34), inference(avatar_split_clause, [], [f913, f370, f438])).
fof(f913, plain, (~ (e1 = op(e0, e3)) | ~ spl4_34), inference(forward_demodulation, [], [f129, f372])).
fof(f898, plain, (~ spl4_34 | ~ spl4_36), inference(avatar_contradiction_clause, [], [f897])).
fof(f897, plain, ($false | (~ spl4_34 | ~ spl4_36)), inference(subsumption_resolution, [], [f896, f163])).
fof(f896, plain, ((e1 = e3) | (~ spl4_34 | ~ spl4_36)), inference(forward_demodulation, [], [f380, f372])).
fof(f380, plain, ((e3 = op(e1, e3)) | ~ spl4_36), inference(avatar_component_clause, [], [f378])).
fof(f891, plain, (~ spl4_30 | ~ spl4_18), inference(avatar_split_clause, [], [f890, f302, f353])).
fof(f890, plain, (~ (e1 = op(e2, e0)) | ~ spl4_18), inference(forward_demodulation, [], [f149, f304])).
fof(f304, plain, ((e1 = op(e2, e3)) | ~ spl4_18), inference(avatar_component_clause, [], [f302])).
fof(f887, plain, (~ spl4_23 | ~ spl4_27), inference(avatar_split_clause, [], [f886, f340, f323])).
fof(f886, plain, (~ (e2 = op(e2, e2)) | ~ spl4_27), inference(forward_demodulation, [], [f150, f342])).
fof(f870, plain, (~ spl4_6 | ~ spl4_8), inference(avatar_contradiction_clause, [], [f869])).
fof(f869, plain, ($false | (~ spl4_6 | ~ spl4_8)), inference(subsumption_resolution, [], [f868, f163])).
fof(f868, plain, ((e1 = e3) | (~ spl4_6 | ~ spl4_8)), inference(forward_demodulation, [], [f261, f253])).
fof(f253, plain, ((e1 = op(e3, e2)) | ~ spl4_6), inference(avatar_component_clause, [], [f251])).
fof(f863, plain, (~ spl4_21 | ~ spl4_3 | spl4_71), inference(avatar_split_clause, [], [f860, f599, f238, f315])).
fof(f860, plain, (~ (e0 = op(e2, e2)) | (~ spl4_3 | spl4_71)), inference(backward_demodulation, [], [f601, f240])).
fof(f856, plain, (~ spl4_6 | ~ spl4_7), inference(avatar_contradiction_clause, [], [f855])).
fof(f855, plain, ($false | (~ spl4_6 | ~ spl4_7)), inference(subsumption_resolution, [], [f854, f162])).
fof(f162, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f854, plain, ((e1 = e2) | (~ spl4_6 | ~ spl4_7)), inference(backward_demodulation, [], [f257, f253])).
fof(f257, plain, ((e2 = op(e3, e2)) | ~ spl4_7), inference(avatar_component_clause, [], [f255])).
fof(f255, plain, (spl4_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_7])])).
fof(f853, plain, (~ spl4_7 | ~ spl4_8), inference(avatar_contradiction_clause, [], [f852])).
fof(f852, plain, ($false | (~ spl4_7 | ~ spl4_8)), inference(subsumption_resolution, [], [f851, f164])).
fof(f851, plain, ((e2 = e3) | (~ spl4_7 | ~ spl4_8)), inference(backward_demodulation, [], [f261, f257])).
fof(f850, plain, (~ spl4_4 | ~ spl4_8), inference(avatar_split_clause, [], [f849, f259, f242])).
fof(f242, plain, (spl4_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_4])])).
fof(f849, plain, (~ (e3 = op(e3, e3)) | ~ spl4_8), inference(backward_demodulation, [], [f158, f261])).
fof(f158, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f840, plain, (~ spl4_15 | ~ spl4_16), inference(avatar_contradiction_clause, [], [f839])).
fof(f839, plain, ($false | (~ spl4_15 | ~ spl4_16)), inference(subsumption_resolution, [], [f838, f164])).
fof(f838, plain, ((e2 = e3) | (~ spl4_15 | ~ spl4_16)), inference(backward_demodulation, [], [f295, f291])).
fof(f837, plain, (~ spl4_4 | ~ spl4_16), inference(avatar_split_clause, [], [f834, f293, f242])).
fof(f834, plain, (~ (e3 = op(e3, e3)) | ~ spl4_16), inference(backward_demodulation, [], [f155, f295])).
fof(f835, plain, (~ spl4_12 | ~ spl4_16), inference(avatar_split_clause, [], [f832, f293, f276])).
fof(f832, plain, (~ (e3 = op(e3, e1)) | ~ spl4_16), inference(backward_demodulation, [], [f153, f295])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f810, plain, (~ spl4_12 | ~ spl4_28), inference(avatar_split_clause, [], [f807, f344, f276])).
fof(f807, plain, (~ (e3 = op(e3, e1)) | ~ spl4_28), inference(backward_demodulation, [], [f122, f346])).
fof(f806, plain, (~ spl4_17 | ~ spl4_29), inference(avatar_split_clause, [], [f802, f349, f298])).
fof(f802, plain, (~ (e0 = op(e2, e3)) | ~ spl4_29), inference(backward_demodulation, [], [f149, f351])).
fof(f798, plain, (~ spl4_34 | ~ spl4_35), inference(avatar_contradiction_clause, [], [f797])).
fof(f797, plain, ($false | (~ spl4_34 | ~ spl4_35)), inference(subsumption_resolution, [], [f796, f162])).
fof(f796, plain, ((e1 = e2) | (~ spl4_34 | ~ spl4_35)), inference(backward_demodulation, [], [f376, f372])).
fof(f795, plain, (~ spl4_35 | ~ spl4_36), inference(avatar_contradiction_clause, [], [f794])).
fof(f794, plain, ($false | (~ spl4_35 | ~ spl4_36)), inference(subsumption_resolution, [], [f793, f164])).
fof(f793, plain, ((e2 = e3) | (~ spl4_35 | ~ spl4_36)), inference(backward_demodulation, [], [f380, f376])).
fof(f735, plain, (~ spl4_53 | ~ spl4_57), inference(avatar_split_clause, [], [f730, f468, f451])).
fof(f730, plain, (~ (e0 = op(e0, e2)) | ~ spl4_57), inference(backward_demodulation, [], [f138, f470])).
fof(f138, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f733, plain, (~ spl4_25 | ~ spl4_57), inference(avatar_split_clause, [], [f728, f468, f332])).
fof(f728, plain, (~ (e0 = op(e2, e1)) | ~ spl4_57), inference(backward_demodulation, [], [f118, f470])).
fof(f723, plain, (~ spl4_49 | ~ spl4_61), inference(avatar_split_clause, [], [f714, f485, f434])).
fof(f485, plain, (spl4_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_61])])).
fof(f714, plain, (~ (e0 = op(e0, e3)) | ~ spl4_61), inference(backward_demodulation, [], [f137, f487])).
fof(f487, plain, ((e0 = op(e0, e0)) | ~ spl4_61), inference(avatar_component_clause, [], [f485])).
fof(f722, plain, (~ spl4_53 | ~ spl4_61), inference(avatar_split_clause, [], [f713, f485, f451])).
fof(f713, plain, (~ (e0 = op(e0, e2)) | ~ spl4_61), inference(backward_demodulation, [], [f136, f487])).
fof(f721, plain, (~ spl4_57 | ~ spl4_61), inference(avatar_split_clause, [], [f712, f485, f468])).
fof(f712, plain, (~ (e0 = op(e0, e1)) | ~ spl4_61), inference(backward_demodulation, [], [f135, f487])).
fof(f707, plain, (~ spl4_92 | ~ spl4_83 | ~ spl4_62), inference(avatar_split_clause, [], [f227, f489, f654, f702])).
fof(f227, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | ~ (e3 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | ~ (e3 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((op(e0, e0) = e1) & (e2 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) & (e3 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax28)).
fof(f706, plain, (~ spl4_91 | ~ spl4_70 | ~ spl4_21), inference(avatar_split_clause, [], [f226, f315, f594, f697])).
fof(f226, plain, (~ (e0 = op(e2, e2)) | ~ (e1 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) | ~ (e3 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e0 = op(e2, e2)) | ~ (e1 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) | ~ (e3 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e0 = op(e2, e2)) & (e1 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) & (e3 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax27)).
fof(f689, plain, (~ spl4_88 | ~ spl4_84 | ~ spl4_62), inference(avatar_split_clause, [], [f221, f489, f660, f680])).
fof(f221, plain, (~ (op(e0, e0) = e1) | ~ (e3 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (op(e0, e0) = e1) | ~ (e3 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((op(e0, e0) = e1) & (e3 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) & (e2 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax22)).
fof(f688, plain, (~ spl4_87 | ~ spl4_72 | ~ spl4_1), inference(avatar_split_clause, [], [f220, f230, f603, f675])).
fof(f220, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | ~ (e2 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | ~ (e2 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e0 = op(e3, e3)) & (e1 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) & (e2 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax21)).
fof(f687, plain, (~ spl4_88 | ~ spl4_89 | ~ spl4_64), inference(avatar_split_clause, [], [f219, f497, f684, f680])).
fof(f219, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((op(e0, e0) = e3) & (e1 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) & (e2 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax20)).
fof(f678, plain, (~ spl4_87 | ~ spl4_81 | ~ spl4_2), inference(avatar_split_clause, [], [f218, f234, f645, f675])).
fof(f218, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | ~ (e2 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | ~ (e2 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e1 = op(e3, e3)) & (e0 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) & (e2 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax19)).
fof(f673, plain, (~ spl4_85 | ~ spl4_86 | ~ spl4_44), inference(avatar_split_clause, [], [f217, f412, f670, f666])).
fof(f217, plain, (~ (e3 = op(e1, e1)) | ~ (e0 = op(op(op(e1, e1), op(e1, e1)), op(e1, e1))) | ~ (e2 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e3 = op(e1, e1)) | ~ (e0 = op(op(op(e1, e1), op(e1, e1)), op(e1, e1))) | ~ (e2 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e3 = op(e1, e1)) & (e0 = op(op(op(e1, e1), op(e1, e1)), op(e1, e1))) & (e2 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax18)).
fof(f657, plain, (~ spl4_82 | ~ spl4_83 | ~ spl4_64), inference(avatar_split_clause, [], [f213, f497, f654, f650])).
fof(f213, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((op(e0, e0) = e3) & (e2 = op(op(op(e0, e0), op(e0, e0)), op(e0, e0))) & (e1 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax14)).
fof(f630, plain, (~ spl4_69 | ~ spl4_77 | ~ spl4_22), inference(avatar_split_clause, [], [f210, f319, f627, f590])).
fof(f210, plain, (~ (e1 = op(e2, e2)) | ~ (e3 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e1 = op(e2, e2)) | ~ (e3 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e1 = op(e2, e2)) & (e3 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) & (e0 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax11)).
fof(f620, plain, (~ spl4_71 | ~ spl4_75 | ~ spl4_2), inference(avatar_split_clause, [], [f208, f234, f617, f599])).
fof(f208, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e1 = op(e3, e3)) & (e2 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) & (e0 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax9)).
fof(f606, plain, (~ spl4_71 | ~ spl4_72 | ~ spl4_3), inference(avatar_split_clause, [], [f206, f238, f603, f599])).
fof(f206, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e2 = op(e3, e3)) & (e1 = op(op(op(e3, e3), op(e3, e3)), op(e3, e3))) & (e0 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax7)).
fof(f597, plain, (~ spl4_69 | ~ spl4_70 | ~ spl4_24), inference(avatar_split_clause, [], [f205, f327, f594, f590])).
fof(f205, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e3 = op(e2, e2)) & (e1 = op(op(op(e2, e2), op(e2, e2)), op(e2, e2))) & (e0 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax6)).
fof(f588, plain, (spl4_68 | spl4_46 | spl4_31 | spl4_16), inference(avatar_split_clause, [], [f173, f293, f357, f421, f552])).
fof(f552, plain, (spl4_68 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl4_68])])).
fof(f173, plain, ((e3 = op(e3, e0)) | (e2 = op(e2, e0)) | (e1 = op(e1, e0)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e2 = op(e2, e3))) | (~ (e3 = op(e3, e1)) & (e1 = op(e1, e3))) | sP3) & ((~ (e2 = op(e2, e3)) & (e3 = op(e3, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e1 = op(e1, e2))) | sP2) & ((~ (e1 = op(e1, e3)) & (e3 = op(e3, e1))) | (~ (e1 = op(e1, e2)) & (e2 = op(e2, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | sP1) & ((~ (e0 = op(e0, e3)) & (e3 = op(e3, e0))) | (~ (e0 = op(e0, e2)) & (e2 = op(e2, e0))) | (~ (e0 = op(e0, e1)) & (e1 = op(e1, e0))) | sP0)), inference(definition_folding, [], [f5, e57, e56, e55, e54])).
fof(f54, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((~ (e1 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (~ (e1 = op(e1, e0)) & (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((~ (e2 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (~ (e2 = op(e2, e0)) & (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((~ (e3 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (~ (e3 = op(e3, e0)) & (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f5, plain, (((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e2 = op(e2, e3))) | (~ (e3 = op(e3, e1)) & (e1 = op(e1, e3))) | (~ (e3 = op(e3, e0)) & (e0 = op(e0, e3)))) & ((~ (e2 = op(e2, e3)) & (e3 = op(e3, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e1 = op(e1, e2))) | (~ (e2 = op(e2, e0)) & (e0 = op(e0, e2)))) & ((~ (e1 = op(e1, e3)) & (e3 = op(e3, e1))) | (~ (e1 = op(e1, e2)) & (e2 = op(e2, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (e0 = op(e0, e1)))) & ((~ (e0 = op(e0, e3)) & (e3 = op(e3, e0))) | (~ (e0 = op(e0, e2)) & (e2 = op(e2, e0))) | (~ (e0 = op(e0, e1)) & (e1 = op(e1, e0))) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax5)).
fof(f587, plain, (spl4_68 | ~ spl4_57 | spl4_31 | spl4_16), inference(avatar_split_clause, [], [f174, f293, f357, f468, f552])).
fof(f174, plain, ((e3 = op(e3, e0)) | (e2 = op(e2, e0)) | ~ (e0 = op(e0, e1)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f584, plain, (spl4_68 | spl4_46 | spl4_31 | ~ spl4_49), inference(avatar_split_clause, [], [f177, f434, f357, f421, f552])).
fof(f177, plain, (~ (e0 = op(e0, e3)) | (e2 = op(e2, e0)) | (e1 = op(e1, e0)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f580, plain, (spl4_67 | spl4_42 | spl4_27 | spl4_12), inference(avatar_split_clause, [], [f181, f276, f340, f404, f546])).
fof(f546, plain, (spl4_67 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl4_67])])).
fof(f181, plain, ((e3 = op(e3, e1)) | (e2 = op(e2, e1)) | (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f579, plain, (spl4_67 | ~ spl4_42 | spl4_27 | spl4_12), inference(avatar_split_clause, [], [f182, f276, f340, f404, f546])).
fof(f182, plain, ((e3 = op(e3, e1)) | (e2 = op(e2, e1)) | ~ (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f578, plain, (spl4_67 | spl4_42 | ~ spl4_38 | spl4_12), inference(avatar_split_clause, [], [f183, f276, f387, f404, f546])).
fof(f183, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f576, plain, (spl4_67 | spl4_42 | spl4_27 | ~ spl4_34), inference(avatar_split_clause, [], [f185, f370, f340, f404, f546])).
fof(f185, plain, (~ (e1 = op(e1, e3)) | (e2 = op(e2, e1)) | (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f572, plain, (spl4_66 | spl4_38 | spl4_23 | spl4_8), inference(avatar_split_clause, [], [f189, f259, f323, f387, f540])).
fof(f540, plain, (spl4_66 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl4_66])])).
fof(f189, plain, ((e3 = op(e3, e2)) | (e2 = op(e2, e2)) | (e1 = op(e1, e2)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f570, plain, (spl4_66 | spl4_38 | ~ spl4_23 | spl4_8), inference(avatar_split_clause, [], [f191, f259, f323, f387, f540])).
fof(f191, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e2, e2)) | (e1 = op(e1, e2)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f568, plain, (spl4_66 | spl4_38 | spl4_23 | ~ spl4_19), inference(avatar_split_clause, [], [f193, f306, f323, f387, f540])).
fof(f193, plain, (~ (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e2)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f564, plain, (spl4_65 | spl4_34 | spl4_19 | spl4_4), inference(avatar_split_clause, [], [f197, f242, f306, f370, f534])).
fof(f534, plain, (spl4_65 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl4_65])])).
fof(f197, plain, ((e3 = op(e3, e3)) | (e2 = op(e2, e3)) | (e1 = op(e1, e3)) | sP3), inference(cnf_transformation, [], [f58])).
fof(f560, plain, (spl4_65 | spl4_34 | spl4_19 | ~ spl4_4), inference(avatar_split_clause, [], [f201, f242, f306, f370, f534])).
fof(f201, plain, (~ (e3 = op(e3, e3)) | (e2 = op(e2, e3)) | (e1 = op(e1, e3)) | sP3), inference(cnf_transformation, [], [f58])).
fof(f556, plain, (~ spl4_68 | spl4_61), inference(avatar_split_clause, [], [f171, f485, f552])).
fof(f171, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f555, plain, (~ spl4_68 | ~ spl4_61), inference(avatar_split_clause, [], [f172, f485, f552])).
fof(f172, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f550, plain, (~ spl4_67 | spl4_57), inference(avatar_split_clause, [], [f169, f468, f546])).
fof(f169, plain, ((e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e1 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f549, plain, (~ spl4_67 | ~ spl4_46), inference(avatar_split_clause, [], [f170, f421, f546])).
fof(f170, plain, (~ (e1 = op(e1, e0)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f544, plain, (~ spl4_66 | spl4_53), inference(avatar_split_clause, [], [f167, f451, f540])).
fof(f167, plain, ((e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e2 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f543, plain, (~ spl4_66 | ~ spl4_31), inference(avatar_split_clause, [], [f168, f357, f540])).
fof(f168, plain, (~ (e2 = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f538, plain, (~ spl4_65 | spl4_49), inference(avatar_split_clause, [], [f165, f434, f534])).
fof(f165, plain, ((e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e3 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f537, plain, (~ spl4_65 | ~ spl4_16), inference(avatar_split_clause, [], [f166, f293, f534])).
fof(f166, plain, (~ (e3 = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f527, plain, (spl4_63 | spl4_47 | spl4_31 | spl4_15), inference(avatar_split_clause, [], [f84, f289, f357, f425, f493])).
fof(f84, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax2)).
fof(f525, plain, (spl4_64 | spl4_48 | spl4_32 | spl4_16), inference(avatar_split_clause, [], [f86, f293, f361, f429, f497])).
fof(f86, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f524, plain, (spl4_45 | spl4_41 | spl4_37 | spl4_33), inference(avatar_split_clause, [], [f87, f366, f383, f400, f417])).
fof(f87, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f520, plain, (spl4_47 | spl4_43 | spl4_39 | spl4_35), inference(avatar_split_clause, [], [f91, f374, f391, f408, f425])).
fof(f91, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f519, plain, (spl4_59 | spl4_43 | spl4_27 | spl4_11), inference(avatar_split_clause, [], [f92, f272, f340, f408, f476])).
fof(f92, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f518, plain, (spl4_48 | spl4_44 | spl4_40 | spl4_36), inference(avatar_split_clause, [], [f93, f378, f395, f412, f429])).
fof(f93, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f516, plain, (spl4_29 | spl4_25 | spl4_21 | spl4_17), inference(avatar_split_clause, [], [f95, f298, f315, f332, f349])).
fof(f95, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f508, plain, (spl4_13 | spl4_9 | spl4_5 | spl4_1), inference(avatar_split_clause, [], [f103, f230, f247, f264, f281])).
fof(f103, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f507, plain, (spl4_49 | spl4_33 | spl4_17 | spl4_1), inference(avatar_split_clause, [], [f104, f230, f298, f366, f434])).
fof(f104, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f506, plain, (spl4_14 | spl4_10 | spl4_6 | spl4_2), inference(avatar_split_clause, [], [f105, f234, f251, f268, f285])).
fof(f105, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f504, plain, (spl4_15 | spl4_11 | spl4_7 | spl4_3), inference(avatar_split_clause, [], [f107, f238, f255, f272, f289])).
fof(f107, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f503, plain, (spl4_51 | spl4_35 | spl4_19 | spl4_3), inference(avatar_split_clause, [], [f108, f238, f306, f374, f442])).
fof(f108, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f501, plain, (spl4_52 | spl4_36 | spl4_20 | spl4_4), inference(avatar_split_clause, [], [f110, f242, f310, f378, f446])).
fof(f110, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f500, plain, (spl4_61 | spl4_62 | spl4_63 | spl4_64), inference(avatar_split_clause, [], [f63, f497, f493, f489, f485])).
fof(f63, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG135+1.p', ax1)).
fof(f483, plain, (spl4_57 | spl4_58 | spl4_59 | spl4_60), inference(avatar_split_clause, [], [f64, f480, f476, f472, f468])).
fof(f64, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f466, plain, (spl4_53 | spl4_54 | spl4_55 | spl4_56), inference(avatar_split_clause, [], [f65, f463, f459, f455, f451])).
fof(f65, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f449, plain, (spl4_49 | spl4_50 | spl4_51 | spl4_52), inference(avatar_split_clause, [], [f66, f446, f442, f438, f434])).
fof(f66, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f432, plain, (spl4_45 | spl4_46 | spl4_47 | spl4_48), inference(avatar_split_clause, [], [f67, f429, f425, f421, f417])).
fof(f67, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f415, plain, (spl4_41 | spl4_42 | spl4_43 | spl4_44), inference(avatar_split_clause, [], [f68, f412, f408, f404, f400])).
fof(f68, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f398, plain, (spl4_37 | spl4_38 | spl4_39 | spl4_40), inference(avatar_split_clause, [], [f69, f395, f391, f387, f383])).
fof(f69, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f364, plain, (spl4_29 | spl4_30 | spl4_31 | spl4_32), inference(avatar_split_clause, [], [f71, f361, f357, f353, f349])).
fof(f71, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f347, plain, (spl4_25 | spl4_26 | spl4_27 | spl4_28), inference(avatar_split_clause, [], [f72, f344, f340, f336, f332])).
fof(f72, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f330, plain, (spl4_21 | spl4_22 | spl4_23 | spl4_24), inference(avatar_split_clause, [], [f73, f327, f323, f319, f315])).
fof(f73, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f313, plain, (spl4_17 | spl4_18 | spl4_19 | spl4_20), inference(avatar_split_clause, [], [f74, f310, f306, f302, f298])).
fof(f74, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f296, plain, (spl4_13 | spl4_14 | spl4_15 | spl4_16), inference(avatar_split_clause, [], [f75, f293, f289, f285, f281])).
fof(f75, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f245, plain, (spl4_1 | spl4_2 | spl4_3 | spl4_4), inference(avatar_split_clause, [], [f78, f242, f238, f234, f230])).
fof(f78, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).