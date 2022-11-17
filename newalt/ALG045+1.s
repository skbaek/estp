fof(f1124, plain, $false, inference(avatar_sat_refutation, [], [f450, f501, f504, f505, f506, f507, f538, f539, f540, f546, f548, f549, f554, f555, f556, f557, f559, f560, f561, f564, f567, f568, f573, f579, f585, f599, f600, f612, f613, f620, f631, f633, f667, f686, f690, f710, f755, f767, f787, f795, f804, f811, f814, f837, f841, f876, f877, f878, f882, f939, f949, f952, f960, f987, f1029, f1053, f1061, f1071, f1086, f1087, f1109, f1123])).
fof(f1123, plain, (~ spl3_2 | ~ spl3_3), inference(avatar_contradiction_clause, [], [f1122])).
fof(f1122, plain, ($false | (~ spl3_2 | ~ spl3_3)), inference(subsumption_resolution, [], [f1121, f170])).
fof(f170, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax5)).
fof(f1121, plain, ((e1 = e2) | (~ spl3_2 | ~ spl3_3)), inference(forward_demodulation, [], [f224, f220])).
fof(f220, plain, ((e1 = op(e3, e3)) | ~ spl3_2), inference(avatar_component_clause, [], [f218])).
fof(f218, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f224, plain, ((e2 = op(e3, e3)) | ~ spl3_3), inference(avatar_component_clause, [], [f222])).
fof(f222, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f1109, plain, (~ spl3_42 | ~ spl3_43), inference(avatar_contradiction_clause, [], [f1108])).
fof(f1108, plain, ($false | (~ spl3_42 | ~ spl3_43)), inference(subsumption_resolution, [], [f1107, f170])).
fof(f1107, plain, ((e1 = e2) | (~ spl3_42 | ~ spl3_43)), inference(forward_demodulation, [], [f394, f390])).
fof(f390, plain, ((e1 = op(e1, e1)) | ~ spl3_42), inference(avatar_component_clause, [], [f388])).
fof(f388, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f394, plain, ((e2 = op(e1, e1)) | ~ spl3_43), inference(avatar_component_clause, [], [f392])).
fof(f392, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f1087, plain, (spl3_46 | ~ spl3_65), inference(avatar_split_clause, [], [f1076, f486, f405])).
fof(f405, plain, (spl3_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_46])])).
fof(f486, plain, (spl3_65 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_65])])).
fof(f1076, plain, ((e1 = op(e1, e0)) | ~ spl3_65), inference(backward_demodulation, [], [f81, f488])).
fof(f488, plain, ((e0 = unit) | ~ spl3_65), inference(avatar_component_clause, [], [f486])).
fof(f81, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax2)).
fof(f1086, plain, (spl3_58 | ~ spl3_65), inference(avatar_split_clause, [], [f1075, f486, f456])).
fof(f456, plain, (spl3_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_58])])).
fof(f1075, plain, ((e1 = op(e0, e1)) | ~ spl3_65), inference(backward_demodulation, [], [f80, f488])).
fof(f80, plain, (e1 = op(unit, e1)), inference(cnf_transformation, [], [f2])).
fof(f1071, plain, (~ spl3_58 | ~ spl3_42), inference(avatar_split_clause, [], [f1070, f388, f456])).
fof(f1070, plain, (~ (e1 = op(e0, e1)) | ~ spl3_42), inference(forward_demodulation, [], [f125, f390])).
fof(f125, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax4)).
fof(f1061, plain, (~ spl3_61 | ~ spl3_49), inference(avatar_split_clause, [], [f1060, f418, f469])).
fof(f469, plain, (spl3_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f418, plain, (spl3_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_49])])).
fof(f1060, plain, (~ (e0 = op(e0, e0)) | ~ spl3_49), inference(forward_demodulation, [], [f145, f420])).
fof(f420, plain, ((e0 = op(e0, e3)) | ~ spl3_49), inference(avatar_component_clause, [], [f418])).
fof(f145, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f1053, plain, (~ spl3_34 | ~ spl3_42), inference(avatar_contradiction_clause, [], [f1052])).
fof(f1052, plain, ($false | (~ spl3_34 | ~ spl3_42)), inference(subsumption_resolution, [], [f1051, f390])).
fof(f1051, plain, (~ (e1 = op(e1, e1)) | ~ spl3_34), inference(forward_demodulation, [], [f153, f356])).
fof(f356, plain, ((e1 = op(e1, e3)) | ~ spl3_34), inference(avatar_component_clause, [], [f354])).
fof(f354, plain, (spl3_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_34])])).
fof(f153, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1029, plain, (~ spl3_58 | ~ spl3_10), inference(avatar_split_clause, [], [f1028, f252, f456])).
fof(f252, plain, (spl3_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_10])])).
fof(f1028, plain, (~ (e1 = op(e0, e1)) | ~ spl3_10), inference(forward_demodulation, [], [f127, f254])).
fof(f254, plain, ((e1 = op(e3, e1)) | ~ spl3_10), inference(avatar_component_clause, [], [f252])).
fof(f127, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f987, plain, (~ spl3_38 | ~ spl3_42), inference(avatar_split_clause, [], [f984, f388, f371])).
fof(f371, plain, (spl3_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_38])])).
fof(f984, plain, (~ (e1 = op(e1, e2)) | ~ spl3_42), inference(backward_demodulation, [], [f152, f390])).
fof(f152, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f4])).
fof(f960, plain, (spl3_49 | ~ spl3_68), inference(avatar_split_clause, [], [f918, f498, f418])).
fof(f498, plain, (spl3_68 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_68])])).
fof(f918, plain, ((e0 = op(e0, e3)) | ~ spl3_68), inference(backward_demodulation, [], [f79, f500])).
fof(f500, plain, ((e3 = unit) | ~ spl3_68), inference(avatar_component_clause, [], [f498])).
fof(f79, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f952, plain, (spl3_34 | ~ spl3_68), inference(avatar_split_clause, [], [f920, f498, f354])).
fof(f920, plain, ((e1 = op(e1, e3)) | ~ spl3_68), inference(backward_demodulation, [], [f81, f500])).
fof(f949, plain, (~ spl3_23 | ~ spl3_31), inference(avatar_split_clause, [], [f948, f341, f307])).
fof(f307, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f341, plain, (spl3_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_31])])).
fof(f948, plain, (~ (e2 = op(e2, e2)) | ~ spl3_31), inference(forward_demodulation, [], [f156, f343])).
fof(f343, plain, ((e2 = op(e2, e0)) | ~ spl3_31), inference(avatar_component_clause, [], [f341])).
fof(f156, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f939, plain, (spl3_7 | ~ spl3_68), inference(avatar_split_clause, [], [f921, f498, f239])).
fof(f239, plain, (spl3_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_7])])).
fof(f921, plain, ((e2 = op(e3, e2)) | ~ spl3_68), inference(backward_demodulation, [], [f82, f500])).
fof(f82, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f882, plain, (spl3_20 | ~ spl3_67), inference(avatar_contradiction_clause, [], [f881])).
fof(f881, plain, ($false | (spl3_20 | ~ spl3_67)), inference(subsumption_resolution, [], [f873, f295])).
fof(f295, plain, (~ (e3 = op(e2, e3)) | spl3_20), inference(avatar_component_clause, [], [f294])).
fof(f294, plain, (spl3_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_20])])).
fof(f873, plain, ((e3 = op(e2, e3)) | ~ spl3_67), inference(backward_demodulation, [], [f84, f496])).
fof(f496, plain, ((e2 = unit) | ~ spl3_67), inference(avatar_component_clause, [], [f494])).
fof(f494, plain, (spl3_67 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_67])])).
fof(f84, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f878, plain, (spl3_38 | ~ spl3_67), inference(avatar_split_clause, [], [f870, f494, f371])).
fof(f870, plain, ((e1 = op(e1, e2)) | ~ spl3_67), inference(backward_demodulation, [], [f81, f496])).
fof(f877, plain, (spl3_26 | ~ spl3_67), inference(avatar_split_clause, [], [f869, f494, f320])).
fof(f320, plain, (spl3_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_26])])).
fof(f869, plain, ((e1 = op(e2, e1)) | ~ spl3_67), inference(backward_demodulation, [], [f80, f496])).
fof(f876, plain, (spl3_53 | ~ spl3_67), inference(avatar_split_clause, [], [f868, f494, f435])).
fof(f435, plain, (spl3_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_53])])).
fof(f868, plain, ((e0 = op(e0, e2)) | ~ spl3_67), inference(backward_demodulation, [], [f79, f496])).
fof(f841, plain, (~ spl3_7 | ~ spl3_39), inference(avatar_split_clause, [], [f838, f375, f239])).
fof(f375, plain, (spl3_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_39])])).
fof(f838, plain, (~ (e2 = op(e3, e2)) | ~ spl3_39), inference(backward_demodulation, [], [f135, f377])).
fof(f377, plain, ((e2 = op(e1, e2)) | ~ spl3_39), inference(avatar_component_clause, [], [f375])).
fof(f135, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f837, plain, (~ spl3_42 | ~ spl3_44), inference(avatar_contradiction_clause, [], [f836])).
fof(f836, plain, ($false | (~ spl3_42 | ~ spl3_44)), inference(subsumption_resolution, [], [f835, f171])).
fof(f171, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f835, plain, ((e1 = e3) | (~ spl3_42 | ~ spl3_44)), inference(backward_demodulation, [], [f398, f390])).
fof(f398, plain, ((e3 = op(e1, e1)) | ~ spl3_44), inference(avatar_component_clause, [], [f396])).
fof(f396, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f814, plain, (spl3_57 | ~ spl3_66), inference(avatar_split_clause, [], [f747, f490, f452])).
fof(f452, plain, (spl3_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_57])])).
fof(f490, plain, (spl3_66 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_66])])).
fof(f747, plain, ((e0 = op(e0, e1)) | ~ spl3_66), inference(backward_demodulation, [], [f79, f492])).
fof(f492, plain, ((e1 = unit) | ~ spl3_66), inference(avatar_component_clause, [], [f490])).
fof(f811, plain, (~ spl3_46 | ~ spl3_14), inference(avatar_split_clause, [], [f810, f269, f405])).
fof(f269, plain, (spl3_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_14])])).
fof(f810, plain, (~ (e1 = op(e1, e0)) | ~ spl3_14), inference(forward_demodulation, [], [f123, f271])).
fof(f271, plain, ((e1 = op(e3, e0)) | ~ spl3_14), inference(avatar_component_clause, [], [f269])).
fof(f123, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f804, plain, (spl3_39 | ~ spl3_66), inference(avatar_split_clause, [], [f750, f490, f375])).
fof(f750, plain, ((e2 = op(e1, e2)) | ~ spl3_66), inference(backward_demodulation, [], [f82, f492])).
fof(f795, plain, (~ spl3_19 | ~ spl3_31), inference(avatar_split_clause, [], [f794, f341, f290])).
fof(f290, plain, (spl3_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_19])])).
fof(f794, plain, (~ (e2 = op(e2, e3)) | ~ spl3_31), inference(forward_demodulation, [], [f157, f343])).
fof(f157, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f787, plain, (~ spl3_7 | ~ spl3_55), inference(avatar_split_clause, [], [f786, f443, f239])).
fof(f443, plain, (spl3_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_55])])).
fof(f786, plain, (~ (e2 = op(e3, e2)) | ~ spl3_55), inference(forward_demodulation, [], [f133, f445])).
fof(f445, plain, ((e2 = op(e0, e2)) | ~ spl3_55), inference(avatar_component_clause, [], [f443])).
fof(f133, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f767, plain, (~ spl3_11 | ~ spl3_66), inference(avatar_contradiction_clause, [], [f766])).
fof(f766, plain, ($false | (~ spl3_11 | ~ spl3_66)), inference(subsumption_resolution, [], [f765, f172])).
fof(f172, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f765, plain, ((e2 = e3) | (~ spl3_11 | ~ spl3_66)), inference(forward_demodulation, [], [f753, f258])).
fof(f258, plain, ((e2 = op(e3, e1)) | ~ spl3_11), inference(avatar_component_clause, [], [f256])).
fof(f256, plain, (spl3_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_11])])).
fof(f753, plain, ((e3 = op(e3, e1)) | ~ spl3_66), inference(backward_demodulation, [], [f85, f492])).
fof(f85, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f755, plain, (spl3_42 | ~ spl3_66), inference(avatar_split_clause, [], [f749, f490, f388])).
fof(f749, plain, ((e1 = op(e1, e1)) | ~ spl3_66), inference(backward_demodulation, [], [f81, f492])).
fof(f710, plain, (~ spl3_3 | ~ spl3_7), inference(avatar_split_clause, [], [f709, f239, f222])).
fof(f709, plain, (~ (e2 = op(e3, e3)) | ~ spl3_7), inference(backward_demodulation, [], [f166, f241])).
fof(f241, plain, ((e2 = op(e3, e2)) | ~ spl3_7), inference(avatar_component_clause, [], [f239])).
fof(f166, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f690, plain, (~ spl3_20 | ~ spl3_24), inference(avatar_split_clause, [], [f688, f311, f294])).
fof(f311, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f688, plain, (~ (e3 = op(e2, e3)) | ~ spl3_24), inference(backward_demodulation, [], [f160, f313])).
fof(f313, plain, ((e3 = op(e2, e2)) | ~ spl3_24), inference(avatar_component_clause, [], [f311])).
fof(f160, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f686, plain, (~ spl3_18 | ~ spl3_26), inference(avatar_split_clause, [], [f683, f320, f286])).
fof(f286, plain, (spl3_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_18])])).
fof(f683, plain, (~ (e1 = op(e2, e3)) | ~ spl3_26), inference(backward_demodulation, [], [f159, f322])).
fof(f322, plain, ((e1 = op(e2, e1)) | ~ spl3_26), inference(avatar_component_clause, [], [f320])).
fof(f159, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f667, plain, (~ spl3_34 | ~ spl3_38), inference(avatar_split_clause, [], [f664, f371, f354])).
fof(f664, plain, (~ (e1 = op(e1, e3)) | ~ spl3_38), inference(backward_demodulation, [], [f154, f373])).
fof(f373, plain, ((e1 = op(e1, e2)) | ~ spl3_38), inference(avatar_component_clause, [], [f371])).
fof(f154, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f633, plain, (~ spl3_49 | ~ spl3_53), inference(avatar_split_clause, [], [f629, f435, f418])).
fof(f629, plain, (~ (e0 = op(e0, e3)) | ~ spl3_53), inference(backward_demodulation, [], [f148, f437])).
fof(f437, plain, ((e0 = op(e0, e2)) | ~ spl3_53), inference(avatar_component_clause, [], [f435])).
fof(f148, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f631, plain, (~ spl3_21 | ~ spl3_53), inference(avatar_split_clause, [], [f627, f435, f299])).
fof(f299, plain, (spl3_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f627, plain, (~ (e0 = op(e2, e2)) | ~ spl3_53), inference(backward_demodulation, [], [f132, f437])).
fof(f132, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f620, plain, (~ spl3_41 | ~ spl3_57), inference(avatar_split_clause, [], [f615, f452, f384])).
fof(f384, plain, (spl3_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f615, plain, (~ (e0 = op(e1, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f125, f454])).
fof(f454, plain, ((e0 = op(e0, e1)) | ~ spl3_57), inference(avatar_component_clause, [], [f452])).
fof(f613, plain, (~ spl3_52 | ~ spl3_64), inference(avatar_split_clause, [], [f607, f481, f430])).
fof(f430, plain, (spl3_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_52])])).
fof(f481, plain, (spl3_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f607, plain, (~ (e3 = op(e0, e3)) | ~ spl3_64), inference(backward_demodulation, [], [f145, f483])).
fof(f483, plain, ((op(e0, e0) = e3) | ~ spl3_64), inference(avatar_component_clause, [], [f481])).
fof(f612, plain, (~ spl3_56 | ~ spl3_64), inference(avatar_split_clause, [], [f606, f481, f447])).
fof(f447, plain, (spl3_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_56])])).
fof(f606, plain, (~ (e3 = op(e0, e2)) | ~ spl3_64), inference(backward_demodulation, [], [f144, f483])).
fof(f144, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f4])).
fof(f600, plain, (spl3_52 | ~ spl3_65), inference(avatar_split_clause, [], [f592, f486, f430])).
fof(f592, plain, ((e3 = op(e0, e3)) | ~ spl3_65), inference(backward_demodulation, [], [f84, f488])).
fof(f599, plain, (spl3_31 | ~ spl3_65), inference(avatar_split_clause, [], [f591, f486, f341])).
fof(f591, plain, ((e2 = op(e2, e0)) | ~ spl3_65), inference(backward_demodulation, [], [f83, f488])).
fof(f83, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f585, plain, (~ spl3_24 | ~ spl3_54), inference(avatar_split_clause, [], [f212, f439, f311])).
fof(f439, plain, (spl3_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_54])])).
fof(f212, plain, (~ (e1 = op(e0, e2)) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f54])).
fof(f54, plain, (~ (e1 = op(e0, e2)) | ~ (e3 = op(e2, e2))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ~ ((e1 = op(e0, e2)) & (e3 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax30)).
fof(f579, plain, (~ spl3_3 | ~ spl3_50), inference(avatar_split_clause, [], [f206, f422, f222])).
fof(f422, plain, (spl3_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_50])])).
fof(f206, plain, (~ (e1 = op(e0, e3)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e1 = op(e0, e3)) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e1 = op(e0, e3)) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax24)).
fof(f573, plain, (~ spl3_2 | ~ spl3_51), inference(avatar_split_clause, [], [f200, f426, f218])).
fof(f426, plain, (spl3_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_51])])).
fof(f200, plain, (~ (e2 = op(e0, e3)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e2 = op(e0, e3)) | ~ (e1 = op(e3, e3))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e2 = op(e0, e3)) & (e1 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax18)).
fof(f568, plain, (~ spl3_62 | ~ spl3_15), inference(avatar_split_clause, [], [f195, f273, f473])).
fof(f473, plain, (spl3_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f273, plain, (spl3_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_15])])).
fof(f195, plain, (~ (e2 = op(e3, e0)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (e2 = op(e3, e0)) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((e2 = op(e3, e0)) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax13)).
fof(f567, plain, (~ spl3_1 | ~ spl3_35), inference(avatar_split_clause, [], [f194, f358, f214])).
fof(f214, plain, (spl3_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f358, plain, (spl3_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_35])])).
fof(f194, plain, (~ (e2 = op(e1, e3)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e2 = op(e1, e3)) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e2 = op(e1, e3)) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax12)).
fof(f564, plain, (~ spl3_21 | ~ spl3_6), inference(avatar_split_clause, [], [f191, f235, f299])).
fof(f235, plain, (spl3_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_6])])).
fof(f191, plain, (~ (e1 = op(e3, e2)) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e1 = op(e3, e2)) | ~ (e0 = op(e2, e2))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e1 = op(e3, e2)) & (e0 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax9)).
fof(f561, plain, (spl3_71 | spl3_70 | spl3_69 | spl3_64), inference(avatar_split_clause, [], [f185, f481, f535, f543, f551])).
fof(f551, plain, (spl3_71 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_71])])).
fof(f543, plain, (spl3_70 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_70])])).
fof(f535, plain, (spl3_69 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_69])])).
fof(f185, plain, ((op(e0, e0) = e3) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | sP2 | sP1 | sP0), inference(definition_folding, [], [f6, e57, e56, e55])).
fof(f55, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e55])).
fof(e55, plain, (sP0 <=> ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f56, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e56])).
fof(e56, plain, (sP1 <=> ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f57, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e57])).
fof(e57, plain, (sP2 <=> ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f6, plain, (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax6)).
fof(f560, plain, (spl3_71 | spl3_70 | spl3_69 | spl3_44), inference(avatar_split_clause, [], [f186, f396, f535, f543, f551])).
fof(f186, plain, ((e3 = op(e1, e1)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f58])).
fof(f559, plain, (spl3_71 | spl3_70 | spl3_69 | spl3_24), inference(avatar_split_clause, [], [f187, f311, f535, f543, f551])).
fof(f187, plain, ((e3 = op(e2, e2)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f58])).
fof(f557, plain, (~ spl3_71 | spl3_61), inference(avatar_split_clause, [], [f181, f469, f551])).
fof(f181, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f61])).
fof(f61, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f55])).
fof(f556, plain, (~ spl3_71 | spl3_41), inference(avatar_split_clause, [], [f182, f384, f551])).
fof(f182, plain, ((e0 = op(e1, e1)) | ~ sP0), inference(cnf_transformation, [], [f61])).
fof(f555, plain, (~ spl3_71 | spl3_21), inference(avatar_split_clause, [], [f183, f299, f551])).
fof(f183, plain, ((e0 = op(e2, e2)) | ~ sP0), inference(cnf_transformation, [], [f61])).
fof(f554, plain, (~ spl3_71 | spl3_1), inference(avatar_split_clause, [], [f184, f214, f551])).
fof(f184, plain, ((e0 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f61])).
fof(f549, plain, (~ spl3_70 | spl3_62), inference(avatar_split_clause, [], [f177, f473, f543])).
fof(f177, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f60])).
fof(f60, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f56])).
fof(f548, plain, (~ spl3_70 | spl3_42), inference(avatar_split_clause, [], [f178, f388, f543])).
fof(f178, plain, ((e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f60])).
fof(f546, plain, (~ spl3_70 | spl3_2), inference(avatar_split_clause, [], [f180, f218, f543])).
fof(f180, plain, ((e1 = op(e3, e3)) | ~ sP1), inference(cnf_transformation, [], [f60])).
fof(f540, plain, (~ spl3_69 | spl3_43), inference(avatar_split_clause, [], [f174, f392, f535])).
fof(f174, plain, ((e2 = op(e1, e1)) | ~ sP2), inference(cnf_transformation, [], [f59])).
fof(f59, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f57])).
fof(f539, plain, (~ spl3_69 | spl3_23), inference(avatar_split_clause, [], [f175, f307, f535])).
fof(f175, plain, ((e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f59])).
fof(f538, plain, (~ spl3_69 | spl3_3), inference(avatar_split_clause, [], [f176, f222, f535])).
fof(f176, plain, ((e2 = op(e3, e3)) | ~ sP2), inference(cnf_transformation, [], [f59])).
fof(f507, plain, (spl3_14 | spl3_10 | spl3_6 | spl3_2), inference(avatar_split_clause, [], [f113, f218, f235, f252, f269])).
fof(f113, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax3)).
fof(f506, plain, (spl3_50 | spl3_34 | spl3_18 | spl3_2), inference(avatar_split_clause, [], [f114, f218, f286, f354, f422])).
fof(f114, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f3])).
fof(f505, plain, (spl3_15 | spl3_11 | spl3_7 | spl3_3), inference(avatar_split_clause, [], [f115, f222, f239, f256, f273])).
fof(f115, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f504, plain, (spl3_51 | spl3_35 | spl3_19 | spl3_3), inference(avatar_split_clause, [], [f116, f222, f290, f358, f426])).
fof(f116, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f3])).
fof(f501, plain, (spl3_65 | spl3_66 | spl3_67 | spl3_68), inference(avatar_split_clause, [], [f86, f498, f494, f490, f486])).
fof(f86, plain, ((e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f450, plain, (spl3_53 | spl3_54 | spl3_55 | spl3_56), inference(avatar_split_clause, [], [f64, f447, f443, f439, f435])).
fof(f64, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG045+1.p', ax1)).