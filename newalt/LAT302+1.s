fof(f3693, plain, $false, inference(avatar_sat_refutation, [], [f862, f867, f877, f884, f901, f960, f1231, f1234, f1559, f2040, f2055, f2058, f2083, f2092, f3692])).
fof(f3692, plain, (spl29_12 | ~ spl29_13 | ~ spl29_14 | ~ spl29_15 | ~ spl29_18 | ~ spl29_74 | spl29_87), inference(avatar_contradiction_clause, [], [f3691])).
fof(f3691, plain, ($false | (spl29_12 | ~ spl29_13 | ~ spl29_14 | ~ spl29_15 | ~ spl29_18 | ~ spl29_74 | spl29_87)), inference(subsumption_resolution, [], [f3690, f1674])).
fof(f1674, plain, (m2_filter_2(k1_tarski(k5_lattices(sK1)), sK1) | (spl29_12 | ~ spl29_13 | ~ spl29_14 | ~ spl29_15 | ~ spl29_18 | spl29_87)), inference(subsumption_resolution, [], [f1673, f267])).
fof(f267, plain, ~ v3_struct_0(sK1), inference(cnf_transformation, [], [f207])).
fof(f207, plain, (~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), k5_lattices(sK1)), sK1) & v13_lattices(sK1) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f108, f206])).
fof(f206, plain, (? [X0] : (~ m2_filter_2(k6_domain_1(u1_struct_0(X0), k5_lattices(X0)), X0) & v13_lattices(X0) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), k5_lattices(sK1)), sK1) & v13_lattices(sK1) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1))), introduced(choice_axiom, [])).
fof(f108, plain, ? [X0] : (~ m2_filter_2(k6_domain_1(u1_struct_0(X0), k5_lattices(X0)), X0) & v13_lattices(X0) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(flattening, [], [f107])).
fof(f107, plain, ? [X0] : ((~ m2_filter_2(k6_domain_1(u1_struct_0(X0), k5_lattices(X0)), X0) & v13_lattices(X0)) & (l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v13_lattices(X0) => m2_filter_2(k6_domain_1(u1_struct_0(X0), k5_lattices(X0)), X0))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v13_lattices(X0) => m2_filter_2(k6_domain_1(u1_struct_0(X0), k5_lattices(X0)), X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', t26_filter_2)).
fof(f1673, plain, (m2_filter_2(k1_tarski(k5_lattices(sK1)), sK1) | v3_struct_0(sK1) | (spl29_12 | ~ spl29_13 | ~ spl29_14 | ~ spl29_15 | ~ spl29_18 | spl29_87)), inference(subsumption_resolution, [], [f1672, f268])).
fof(f268, plain, v10_lattices(sK1), inference(cnf_transformation, [], [f207])).
fof(f1672, plain, (m2_filter_2(k1_tarski(k5_lattices(sK1)), sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | (spl29_12 | ~ spl29_13 | ~ spl29_14 | ~ spl29_15 | ~ spl29_18 | spl29_87)), inference(subsumption_resolution, [], [f1668, f269])).
fof(f269, plain, l3_lattices(sK1), inference(cnf_transformation, [], [f207])).
fof(f1668, plain, (m2_filter_2(k1_tarski(k5_lattices(sK1)), sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | (spl29_12 | ~ spl29_13 | ~ spl29_14 | ~ spl29_15 | ~ spl29_18 | spl29_87)), inference(resolution, [], [f1650, f426])).
fof(f426, plain, ! [X0, X1] : (~ m1_filter_2(X1, k1_lattice2(X0)) | m2_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f265])).
fof(f265, plain, ! [X0] : (! [X1] : ((m2_filter_2(X1, X0) | ~ m1_filter_2(X1, k1_lattice2(X0))) & (m1_filter_2(X1, k1_lattice2(X0)) | ~ m2_filter_2(X1, X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f190])).
fof(f190, plain, ! [X0] : (! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f189])).
fof(f189, plain, ! [X0] : (! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', t21_filter_2)).
fof(f1650, plain, (m1_filter_2(k1_tarski(k5_lattices(sK1)), k1_lattice2(sK1)) | (spl29_12 | ~ spl29_13 | ~ spl29_14 | ~ spl29_15 | ~ spl29_18 | spl29_87)), inference(forward_demodulation, [], [f1649, f1621])).
fof(f1621, plain, ((k6_domain_1(u1_struct_0(k1_lattice2(sK1)), k5_lattices(sK1)) = k1_tarski(k5_lattices(sK1))) | (~ spl29_18 | spl29_87)), inference(subsumption_resolution, [], [f1619, f1431])).
fof(f1431, plain, (~ v1_xboole_0(u1_struct_0(k1_lattice2(sK1))) | spl29_87), inference(avatar_component_clause, [], [f1430])).
fof(f1430, plain, (spl29_87 <=> v1_xboole_0(u1_struct_0(k1_lattice2(sK1)))), introduced(avatar_definition, [new_symbols(naming, [spl29_87])])).
fof(f1619, plain, ((k6_domain_1(u1_struct_0(k1_lattice2(sK1)), k5_lattices(sK1)) = k1_tarski(k5_lattices(sK1))) | v1_xboole_0(u1_struct_0(k1_lattice2(sK1))) | ~ spl29_18), inference(resolution, [], [f861, f417])).
fof(f417, plain, ! [X0, X1] : (~ m1_subset_1(X1, X0) | (k6_domain_1(X0, X1) = k1_tarski(X1)) | v1_xboole_0(X0)), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ! [X0, X1] : ((k6_domain_1(X0, X1) = k1_tarski(X1)) | ~ m1_subset_1(X1, X0) | v1_xboole_0(X0)), inference(flattening, [], [f182])).
fof(f182, plain, ! [X0, X1] : ((k6_domain_1(X0, X1) = k1_tarski(X1)) | (~ m1_subset_1(X1, X0) | v1_xboole_0(X0))), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1] : ((m1_subset_1(X1, X0) & ~ v1_xboole_0(X0)) => (k6_domain_1(X0, X1) = k1_tarski(X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', redefinition_k6_domain_1)).
fof(f861, plain, (m1_subset_1(k5_lattices(sK1), u1_struct_0(k1_lattice2(sK1))) | ~ spl29_18), inference(avatar_component_clause, [], [f859])).
fof(f859, plain, (spl29_18 <=> m1_subset_1(k5_lattices(sK1), u1_struct_0(k1_lattice2(sK1)))), introduced(avatar_definition, [new_symbols(naming, [spl29_18])])).
fof(f1649, plain, (m1_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(sK1)), k5_lattices(sK1)), k1_lattice2(sK1)) | (spl29_12 | ~ spl29_13 | ~ spl29_14 | ~ spl29_15)), inference(subsumption_resolution, [], [f1648, f847])).
fof(f847, plain, (l3_lattices(k1_lattice2(sK1)) | ~ spl29_15), inference(avatar_component_clause, [], [f846])).
fof(f846, plain, (spl29_15 <=> l3_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_15])])).
fof(f1648, plain, (m1_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(sK1)), k5_lattices(sK1)), k1_lattice2(sK1)) | ~ l3_lattices(k1_lattice2(sK1)) | (spl29_12 | ~ spl29_13 | ~ spl29_14)), inference(subsumption_resolution, [], [f1647, f835])).
fof(f835, plain, (~ v3_struct_0(k1_lattice2(sK1)) | spl29_12), inference(avatar_component_clause, [], [f834])).
fof(f834, plain, (spl29_12 <=> v3_struct_0(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_12])])).
fof(f1647, plain, (m1_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(sK1)), k5_lattices(sK1)), k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ l3_lattices(k1_lattice2(sK1)) | (~ spl29_13 | ~ spl29_14)), inference(subsumption_resolution, [], [f1646, f839])).
fof(f839, plain, (v10_lattices(k1_lattice2(sK1)) | ~ spl29_13), inference(avatar_component_clause, [], [f838])).
fof(f838, plain, (spl29_13 <=> v10_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_13])])).
fof(f1646, plain, (m1_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(sK1)), k5_lattices(sK1)), k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ l3_lattices(k1_lattice2(sK1)) | ~ spl29_14), inference(subsumption_resolution, [], [f1639, f843])).
fof(f843, plain, (v14_lattices(k1_lattice2(sK1)) | ~ spl29_14), inference(avatar_component_clause, [], [f842])).
fof(f842, plain, (spl29_14 <=> v14_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_14])])).
fof(f1639, plain, (m1_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(sK1)), k5_lattices(sK1)), k1_lattice2(sK1)) | ~ v14_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ l3_lattices(k1_lattice2(sK1))), inference(superposition, [], [f822, f812])).
fof(f812, plain, (k5_lattices(sK1) = k6_lattices(k1_lattice2(sK1))), inference(subsumption_resolution, [], [f811, f267])).
fof(f811, plain, ((k5_lattices(sK1) = k6_lattices(k1_lattice2(sK1))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f810, f268])).
fof(f810, plain, ((k5_lattices(sK1) = k6_lattices(k1_lattice2(sK1))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f808, f269])).
fof(f808, plain, (~ l3_lattices(sK1) | (k5_lattices(sK1) = k6_lattices(k1_lattice2(sK1))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f434, f270])).
fof(f270, plain, v13_lattices(sK1), inference(cnf_transformation, [], [f207])).
fof(f434, plain, ! [X0] : (~ v13_lattices(X0) | ~ l3_lattices(X0) | (k5_lattices(X0) = k6_lattices(k1_lattice2(X0))) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f201])).
fof(f201, plain, ! [X0] : ((k5_lattices(X0) = k6_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v13_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f200])).
fof(f200, plain, ! [X0] : ((k5_lattices(X0) = k6_lattices(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v13_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f91])).
fof(f91, plain, ! [X0] : ((l3_lattices(X0) & v13_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (k5_lattices(X0) = k6_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', t78_lattice2)).
fof(f822, plain, ! [X0] : (m1_filter_2(k6_domain_1(u1_struct_0(X0), k6_lattices(X0)), X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0)), inference(duplicate_literal_removal, [], [f819])).
fof(f819, plain, ! [X0] : (~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | m1_filter_2(k6_domain_1(u1_struct_0(X0), k6_lattices(X0)), X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(resolution, [], [f437, f419])).
fof(f419, plain, ! [X0, X1] : (~ m1_filter_0(X1, X0) | m1_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f263])).
fof(f263, plain, ! [X0] : (! [X1] : ((m1_filter_2(X1, X0) | ~ m1_filter_0(X1, X0)) & (m1_filter_0(X1, X0) | ~ m1_filter_2(X1, X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f185])).
fof(f185, plain, ! [X0] : (! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f184])).
fof(f184, plain, ! [X0] : (! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', redefinition_m1_filter_2)).
fof(f437, plain, ! [X0] : (m1_filter_0(k6_domain_1(u1_struct_0(X0), k6_lattices(X0)), X0) | ~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(duplicate_literal_removal, [], [f423])).
fof(f423, plain, ! [X0] : (m1_filter_0(k6_domain_1(u1_struct_0(X0), k6_lattices(X0)), X0) | ~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ! [X0] : (m1_filter_0(k6_domain_1(u1_struct_0(X0), k6_lattices(X0)), X0) | ~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f186])).
fof(f186, plain, ! [X0] : ((m1_filter_0(k6_domain_1(u1_struct_0(X0), k6_lattices(X0)), X0) | (~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f82])).
fof(f82, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ((l3_lattices(X0) & v14_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m1_filter_0(k6_domain_1(u1_struct_0(X0), k6_lattices(X0)), X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', t13_filter_0)).
fof(f3690, plain, (~ m2_filter_2(k1_tarski(k5_lattices(sK1)), sK1) | (~ spl29_18 | ~ spl29_74 | spl29_87)), inference(backward_demodulation, [], [f271, f3683])).
fof(f3683, plain, ((k6_domain_1(u1_struct_0(sK1), k5_lattices(sK1)) = k1_tarski(k5_lattices(sK1))) | (~ spl29_18 | ~ spl29_74 | spl29_87)), inference(backward_demodulation, [], [f1621, f3632])).
fof(f3632, plain, ((u1_struct_0(sK1) = u1_struct_0(k1_lattice2(sK1))) | ~ spl29_74), inference(trivial_inequality_removal, [], [f3615])).
fof(f3615, plain, (~ (k1_lattice2(sK1) = k1_lattice2(sK1)) | (u1_struct_0(sK1) = u1_struct_0(k1_lattice2(sK1))) | ~ spl29_74), inference(superposition, [], [f1230, f1605])).
fof(f1605, plain, (k1_lattice2(sK1) = g3_lattices(u1_struct_0(k1_lattice2(sK1)), u2_lattices(k1_lattice2(sK1)), u1_lattices(k1_lattice2(sK1)))), inference(resolution, [], [f755, f269])).
fof(f755, plain, ! [X1] : (~ l3_lattices(X1) | (k1_lattice2(X1) = g3_lattices(u1_struct_0(k1_lattice2(X1)), u2_lattices(k1_lattice2(X1)), u1_lattices(k1_lattice2(X1))))), inference(subsumption_resolution, [], [f750, f297])).
fof(f297, plain, ! [X0] : (l3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : ((l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (l3_lattices(X0) => (l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', dt_k1_lattice2)).
fof(f750, plain, ! [X1] : ((k1_lattice2(X1) = g3_lattices(u1_struct_0(k1_lattice2(X1)), u2_lattices(k1_lattice2(X1)), u1_lattices(k1_lattice2(X1)))) | ~ l3_lattices(k1_lattice2(X1)) | ~ l3_lattices(X1)), inference(resolution, [], [f272, f296])).
fof(f296, plain, ! [X0] : (v3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f128])).
fof(f272, plain, ! [X0] : (~ v3_lattices(X0) | (g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f110])).
fof(f110, plain, ! [X0] : ((g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ v3_lattices(X0) | ~ l3_lattices(X0)), inference(flattening, [], [f109])).
fof(f109, plain, ! [X0] : (((g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ v3_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (l3_lattices(X0) => (v3_lattices(X0) => (g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', abstractness_v3_lattices)).
fof(f1230, plain, (! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15)) | ~ spl29_74), inference(avatar_component_clause, [], [f1229])).
fof(f1229, plain, (spl29_74 <=> ! [X16, X15, X17] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15))), introduced(avatar_definition, [new_symbols(naming, [spl29_74])])).
fof(f271, plain, ~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), k5_lattices(sK1)), sK1), inference(cnf_transformation, [], [f207])).
fof(f2092, plain, spl29_71, inference(avatar_contradiction_clause, [], [f2091])).
fof(f2091, plain, ($false | spl29_71), inference(subsumption_resolution, [], [f2090, f455])).
fof(f455, plain, l2_lattices(sK1), inference(resolution, [], [f304, f269])).
fof(f304, plain, ! [X0] : (~ l3_lattices(X0) | l2_lattices(X0)), inference(cnf_transformation, [], [f137])).
fof(f137, plain, ! [X0] : ((l2_lattices(X0) & l1_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (l3_lattices(X0) => (l2_lattices(X0) & l1_lattices(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', dt_l3_lattices)).
fof(f2090, plain, (~ l2_lattices(sK1) | spl29_71), inference(resolution, [], [f1219, f731])).
fof(f731, plain, ! [X1] : (m1_relset_1(u2_lattices(X1), k2_zfmisc_1(u1_struct_0(X1), u1_struct_0(X1)), u1_struct_0(X1)) | ~ l2_lattices(X1)), inference(resolution, [], [f318, f420])).
fof(f420, plain, ! [X2, X0, X1] : (~ m2_relset_1(X2, X0, X1) | m1_relset_1(X2, X0, X1)), inference(cnf_transformation, [], [f264])).
fof(f264, plain, ! [X0, X1, X2] : ((m2_relset_1(X2, X0, X1) | ~ m1_relset_1(X2, X0, X1)) & (m1_relset_1(X2, X0, X1) | ~ m2_relset_1(X2, X0, X1))), inference(nnf_transformation, [], [f80])).
fof(f80, plain, ! [X0, X1, X2] : (m2_relset_1(X2, X0, X1) <=> m1_relset_1(X2, X0, X1)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', redefinition_m2_relset_1)).
fof(f318, plain, ! [X0] : (m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : ((m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u2_lattices(X0))) | ~ l2_lattices(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (l2_lattices(X0) => (m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u2_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', dt_u2_lattices)).
fof(f1219, plain, (~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl29_71), inference(avatar_component_clause, [], [f1217])).
fof(f1217, plain, (spl29_71 <=> m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_71])])).
fof(f2083, plain, spl29_70, inference(avatar_contradiction_clause, [], [f2082])).
fof(f2082, plain, ($false | spl29_70), inference(subsumption_resolution, [], [f2081, f455])).
fof(f2081, plain, (~ l2_lattices(sK1) | spl29_70), inference(resolution, [], [f1215, f317])).
fof(f317, plain, ! [X0] : (v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f148])).
fof(f1215, plain, (~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl29_70), inference(avatar_component_clause, [], [f1213])).
fof(f1213, plain, (spl29_70 <=> v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_70])])).
fof(f2058, plain, spl29_69, inference(avatar_contradiction_clause, [], [f2057])).
fof(f2057, plain, ($false | spl29_69), inference(subsumption_resolution, [], [f2056, f455])).
fof(f2056, plain, (~ l2_lattices(sK1) | spl29_69), inference(resolution, [], [f1211, f316])).
fof(f316, plain, ! [X0] : (v1_funct_1(u2_lattices(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f148])).
fof(f1211, plain, (~ v1_funct_1(u2_lattices(sK1)) | spl29_69), inference(avatar_component_clause, [], [f1209])).
fof(f1209, plain, (spl29_69 <=> v1_funct_1(u2_lattices(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_69])])).
fof(f2055, plain, spl29_68, inference(avatar_contradiction_clause, [], [f2054])).
fof(f2054, plain, ($false | spl29_68), inference(subsumption_resolution, [], [f2053, f448])).
fof(f448, plain, l1_lattices(sK1), inference(resolution, [], [f303, f269])).
fof(f303, plain, ! [X0] : (~ l3_lattices(X0) | l1_lattices(X0)), inference(cnf_transformation, [], [f137])).
fof(f2053, plain, (~ l1_lattices(sK1) | spl29_68), inference(resolution, [], [f1207, f729])).
fof(f729, plain, ! [X1] : (m1_relset_1(u1_lattices(X1), k2_zfmisc_1(u1_struct_0(X1), u1_struct_0(X1)), u1_struct_0(X1)) | ~ l1_lattices(X1)), inference(resolution, [], [f315, f420])).
fof(f315, plain, ! [X0] : (m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : ((m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u1_lattices(X0))) | ~ l1_lattices(X0)), inference(ennf_transformation, [], [f34])).
fof(f34, plain, ! [X0] : (l1_lattices(X0) => (m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u1_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', dt_u1_lattices)).
fof(f1207, plain, (~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl29_68), inference(avatar_component_clause, [], [f1205])).
fof(f1205, plain, (spl29_68 <=> m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_68])])).
fof(f2040, plain, spl29_67, inference(avatar_contradiction_clause, [], [f2039])).
fof(f2039, plain, ($false | spl29_67), inference(subsumption_resolution, [], [f2038, f448])).
fof(f2038, plain, (~ l1_lattices(sK1) | spl29_67), inference(resolution, [], [f1203, f314])).
fof(f314, plain, ! [X0] : (v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f147])).
fof(f1203, plain, (~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl29_67), inference(avatar_component_clause, [], [f1201])).
fof(f1201, plain, (spl29_67 <=> v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_67])])).
fof(f1559, plain, (spl29_12 | ~ spl29_17 | ~ spl29_87), inference(avatar_contradiction_clause, [], [f1558])).
fof(f1558, plain, ($false | (spl29_12 | ~ spl29_17 | ~ spl29_87)), inference(subsumption_resolution, [], [f1557, f835])).
fof(f1557, plain, (v3_struct_0(k1_lattice2(sK1)) | (~ spl29_17 | ~ spl29_87)), inference(subsumption_resolution, [], [f1551, f962])).
fof(f962, plain, (l1_struct_0(k1_lattice2(sK1)) | ~ spl29_17), inference(resolution, [], [f856, f302])).
fof(f302, plain, ! [X0] : (~ l2_lattices(X0) | l1_struct_0(X0)), inference(cnf_transformation, [], [f136])).
fof(f136, plain, ! [X0] : (l1_struct_0(X0) | ~ l2_lattices(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (l2_lattices(X0) => l1_struct_0(X0)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', dt_l2_lattices)).
fof(f856, plain, (l2_lattices(k1_lattice2(sK1)) | ~ spl29_17), inference(avatar_component_clause, [], [f855])).
fof(f855, plain, (spl29_17 <=> l2_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_17])])).
fof(f1551, plain, (~ l1_struct_0(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl29_87), inference(resolution, [], [f1432, f332])).
fof(f332, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | ~ l1_struct_0(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | ~ l1_struct_0(X0) | v3_struct_0(X0)), inference(flattening, [], [f159])).
fof(f159, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | (~ l1_struct_0(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : ((l1_struct_0(X0) & ~ v3_struct_0(X0)) => ~ v1_xboole_0(u1_struct_0(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', fc1_struct_0)).
fof(f1432, plain, (v1_xboole_0(u1_struct_0(k1_lattice2(sK1))) | ~ spl29_87), inference(avatar_component_clause, [], [f1430])).
fof(f1234, plain, spl29_66, inference(avatar_contradiction_clause, [], [f1233])).
fof(f1233, plain, ($false | spl29_66), inference(subsumption_resolution, [], [f1232, f448])).
fof(f1232, plain, (~ l1_lattices(sK1) | spl29_66), inference(resolution, [], [f1199, f313])).
fof(f313, plain, ! [X0] : (v1_funct_1(u1_lattices(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f147])).
fof(f1199, plain, (~ v1_funct_1(u1_lattices(sK1)) | spl29_66), inference(avatar_component_clause, [], [f1197])).
fof(f1197, plain, (spl29_66 <=> v1_funct_1(u1_lattices(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl29_66])])).
fof(f1231, plain, (~ spl29_66 | ~ spl29_67 | ~ spl29_68 | ~ spl29_69 | ~ spl29_70 | ~ spl29_71 | spl29_74), inference(avatar_split_clause, [], [f1195, f1229, f1217, f1213, f1209, f1205, f1201, f1197])).
fof(f1195, plain, ! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15) | ~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u2_lattices(sK1)) | ~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u1_lattices(sK1))), inference(superposition, [], [f363, f714])).
fof(f714, plain, (k1_lattice2(sK1) = g3_lattices(u1_struct_0(sK1), u1_lattices(sK1), u2_lattices(sK1))), inference(resolution, [], [f293, f269])).
fof(f293, plain, ! [X0] : (~ l3_lattices(X0) | (k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0)))), inference(cnf_transformation, [], [f125])).
fof(f125, plain, ! [X0] : ((k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (l3_lattices(X0) => (k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', d2_lattice2)).
fof(f363, plain, ! [X4, X2, X0, X5, X3, X1] : (~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5)) | (X0 = X3) | ~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1)), inference(cnf_transformation, [], [f176])).
fof(f176, plain, ! [X0, X1, X2] : (! [X3, X4, X5] : (((X2 = X5) & (X1 = X4) & (X0 = X3)) | ~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5))) | ~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1)), inference(flattening, [], [f175])).
fof(f175, plain, ! [X0, X1, X2] : (! [X3, X4, X5] : (((X2 = X5) & (X1 = X4) & (X0 = X3)) | ~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5))) | (~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1))), inference(ennf_transformation, [], [f61])).
fof(f61, plain, ! [X0, X1, X2] : ((m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) & v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) & v1_funct_1(X2) & m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) & v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) & v1_funct_1(X1)) => ! [X3, X4, X5] : ((g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5)) => ((X2 = X5) & (X1 = X4) & (X0 = X3)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', free_g3_lattices)).
fof(f960, plain, (spl29_17 | ~ spl29_15), inference(avatar_split_clause, [], [f942, f846, f855])).
fof(f942, plain, (l2_lattices(k1_lattice2(sK1)) | ~ spl29_15), inference(resolution, [], [f847, f304])).
fof(f901, plain, ~ spl29_12, inference(avatar_contradiction_clause, [], [f900])).
fof(f900, plain, ($false | ~ spl29_12), inference(subsumption_resolution, [], [f896, f596])).
fof(f596, plain, sP0(sK1), inference(subsumption_resolution, [], [f595, f267])).
fof(f595, plain, (sP0(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f589, f268])).
fof(f589, plain, (sP0(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f362, f269])).
fof(f362, plain, ! [X0] : (~ l3_lattices(X0) | sP0(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f205])).
fof(f205, plain, ! [X0] : (sP0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(definition_folding, [], [f174, e204])).
fof(f204, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(usedef, [], [e204])).
fof(e204, plain, ! [X0] : (sP0(X0) <=> (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f174, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f173])).
fof(f173, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f60])).
fof(f60, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', fc6_lattice2)).
fof(f896, plain, (~ sP0(sK1) | ~ spl29_12), inference(resolution, [], [f836, f353])).
fof(f353, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f230])).
fof(f230, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(nnf_transformation, [], [f204])).
fof(f836, plain, (v3_struct_0(k1_lattice2(sK1)) | ~ spl29_12), inference(avatar_component_clause, [], [f834])).
fof(f884, plain, spl29_15, inference(avatar_contradiction_clause, [], [f883])).
fof(f883, plain, ($false | spl29_15), inference(subsumption_resolution, [], [f882, f269])).
fof(f882, plain, (~ l3_lattices(sK1) | spl29_15), inference(resolution, [], [f848, f297])).
fof(f848, plain, (~ l3_lattices(k1_lattice2(sK1)) | spl29_15), inference(avatar_component_clause, [], [f846])).
fof(f877, plain, spl29_14, inference(avatar_contradiction_clause, [], [f876])).
fof(f876, plain, ($false | spl29_14), inference(subsumption_resolution, [], [f875, f267])).
fof(f875, plain, (v3_struct_0(sK1) | spl29_14), inference(subsumption_resolution, [], [f874, f268])).
fof(f874, plain, (~ v10_lattices(sK1) | v3_struct_0(sK1) | spl29_14), inference(subsumption_resolution, [], [f873, f269])).
fof(f873, plain, (~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | spl29_14), inference(subsumption_resolution, [], [f872, f270])).
fof(f872, plain, (~ v13_lattices(sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | spl29_14), inference(resolution, [], [f844, f431])).
fof(f431, plain, ! [X0] : (v14_lattices(k1_lattice2(X0)) | ~ v13_lattices(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f266])).
fof(f266, plain, ! [X0] : (((v13_lattices(X0) | ~ v14_lattices(k1_lattice2(X0))) & (v14_lattices(k1_lattice2(X0)) | ~ v13_lattices(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f198])).
fof(f198, plain, ! [X0] : ((v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f197])).
fof(f197, plain, ! [X0] : ((v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f89])).
fof(f89, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', t63_lattice2)).
fof(f844, plain, (~ v14_lattices(k1_lattice2(sK1)) | spl29_14), inference(avatar_component_clause, [], [f842])).
fof(f867, plain, spl29_13, inference(avatar_contradiction_clause, [], [f866])).
fof(f866, plain, ($false | spl29_13), inference(subsumption_resolution, [], [f865, f596])).
fof(f865, plain, (~ sP0(sK1) | spl29_13), inference(resolution, [], [f840, f361])).
fof(f361, plain, ! [X0] : (v10_lattices(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f230])).
fof(f840, plain, (~ v10_lattices(k1_lattice2(sK1)) | spl29_13), inference(avatar_component_clause, [], [f838])).
fof(f862, plain, (spl29_12 | ~ spl29_17 | spl29_18), inference(avatar_split_clause, [], [f832, f859, f855, f834])).
fof(f832, plain, (m1_subset_1(k5_lattices(sK1), u1_struct_0(k1_lattice2(sK1))) | ~ l2_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1))), inference(superposition, [], [f300, f812])).
fof(f300, plain, ! [X0] : (m1_subset_1(k6_lattices(X0), u1_struct_0(X0)) | ~ l2_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ! [X0] : (m1_subset_1(k6_lattices(X0), u1_struct_0(X0)) | ~ l2_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f133])).
fof(f133, plain, ! [X0] : (m1_subset_1(k6_lattices(X0), u1_struct_0(X0)) | (~ l2_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : ((l2_lattices(X0) & ~ v3_struct_0(X0)) => m1_subset_1(k6_lattices(X0), u1_struct_0(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT302+1.p', dt_k6_lattices)).