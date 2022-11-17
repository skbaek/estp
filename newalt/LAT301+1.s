fof(f2081, plain, $false, inference(avatar_sat_refutation, [], [f778, f794, f831, f851, f860, f2070])).
fof(f2070, plain, (spl28_11 | ~ spl28_14 | ~ spl28_16 | ~ spl28_17), inference(avatar_contradiction_clause, [], [f2069])).
fof(f2069, plain, ($false | (spl28_11 | ~ spl28_14 | ~ spl28_16 | ~ spl28_17)), inference(subsumption_resolution, [], [f2065, f266])).
fof(f266, plain, ~ r2_hidden(k5_lattices(sK1), sK2), inference(cnf_transformation, [], [f206])).
fof(f206, plain, ((~ r2_hidden(k5_lattices(sK1), sK2) & v13_lattices(sK1) & m2_filter_2(sK2, sK1)) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2])], [f103, f205, f204])).
fof(f204, plain, (? [X0] : (? [X1] : (~ r2_hidden(k5_lattices(X0), X1) & v13_lattices(X0) & m2_filter_2(X1, X0)) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (? [X1] : (~ r2_hidden(k5_lattices(sK1), X1) & v13_lattices(sK1) & m2_filter_2(X1, sK1)) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1))), introduced(choice_axiom, [])).
fof(f205, plain, (? [X1] : (~ r2_hidden(k5_lattices(sK1), X1) & v13_lattices(sK1) & m2_filter_2(X1, sK1)) => (~ r2_hidden(k5_lattices(sK1), sK2) & v13_lattices(sK1) & m2_filter_2(sK2, sK1))), introduced(choice_axiom, [])).
fof(f103, plain, ? [X0] : (? [X1] : (~ r2_hidden(k5_lattices(X0), X1) & v13_lattices(X0) & m2_filter_2(X1, X0)) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(flattening, [], [f102])).
fof(f102, plain, ? [X0] : (? [X1] : ((~ r2_hidden(k5_lattices(X0), X1) & v13_lattices(X0)) & m2_filter_2(X1, X0)) & (l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_filter_2(X1, X0) => (v13_lattices(X0) => r2_hidden(k5_lattices(X0), X1)))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_filter_2(X1, X0) => (v13_lattices(X0) => r2_hidden(k5_lattices(X0), X1)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', t25_filter_2)).
fof(f2065, plain, (r2_hidden(k5_lattices(sK1), sK2) | (spl28_11 | ~ spl28_14 | ~ spl28_16 | ~ spl28_17)), inference(resolution, [], [f793, f1496])).
fof(f1496, plain, (m1_filter_0(sK2, k1_lattice2(sK1)) | (spl28_11 | ~ spl28_14 | ~ spl28_16)), inference(subsumption_resolution, [], [f1495, f740])).
fof(f740, plain, (~ v3_struct_0(k1_lattice2(sK1)) | spl28_11), inference(avatar_component_clause, [], [f739])).
fof(f739, plain, (spl28_11 <=> v3_struct_0(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl28_11])])).
fof(f1495, plain, (m1_filter_0(sK2, k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl28_14 | ~ spl28_16)), inference(subsumption_resolution, [], [f1494, f781])).
fof(f781, plain, (v10_lattices(k1_lattice2(sK1)) | ~ spl28_14), inference(avatar_component_clause, [], [f780])).
fof(f780, plain, (spl28_14 <=> v10_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl28_14])])).
fof(f1494, plain, (m1_filter_0(sK2, k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl28_16), inference(subsumption_resolution, [], [f1491, f789])).
fof(f789, plain, (l3_lattices(k1_lattice2(sK1)) | ~ spl28_16), inference(avatar_component_clause, [], [f788])).
fof(f788, plain, (spl28_16 <=> l3_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl28_16])])).
fof(f1491, plain, (m1_filter_0(sK2, k1_lattice2(sK1)) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1))), inference(resolution, [], [f1489, f408])).
fof(f408, plain, ! [X0, X1] : (~ m1_filter_2(X1, X0) | m1_filter_0(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f258])).
fof(f258, plain, ! [X0] : (! [X1] : ((m1_filter_2(X1, X0) | ~ m1_filter_0(X1, X0)) & (m1_filter_0(X1, X0) | ~ m1_filter_2(X1, X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f185])).
fof(f185, plain, ! [X0] : (! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f184])).
fof(f184, plain, ! [X0] : (! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f76])).
fof(f76, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', redefinition_m1_filter_2)).
fof(f1489, plain, m1_filter_2(sK2, k1_lattice2(sK1)), inference(backward_demodulation, [], [f939, f1488])).
fof(f1488, plain, (sK2 = k7_filter_2(sK1, sK2)), inference(subsumption_resolution, [], [f1487, f261])).
fof(f261, plain, ~ v3_struct_0(sK1), inference(cnf_transformation, [], [f206])).
fof(f1487, plain, (v3_struct_0(sK1) | (sK2 = k7_filter_2(sK1, sK2))), inference(subsumption_resolution, [], [f1486, f262])).
fof(f262, plain, v10_lattices(sK1), inference(cnf_transformation, [], [f206])).
fof(f1486, plain, (~ v10_lattices(sK1) | v3_struct_0(sK1) | (sK2 = k7_filter_2(sK1, sK2))), inference(subsumption_resolution, [], [f1481, f263])).
fof(f263, plain, l3_lattices(sK1), inference(cnf_transformation, [], [f206])).
fof(f1481, plain, (~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | (sK2 = k7_filter_2(sK1, sK2))), inference(resolution, [], [f805, f681])).
fof(f681, plain, m2_lattice4(sK2, sK1), inference(subsumption_resolution, [], [f680, f261])).
fof(f680, plain, (m2_lattice4(sK2, sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f679, f262])).
fof(f679, plain, (m2_lattice4(sK2, sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f676, f263])).
fof(f676, plain, (m2_lattice4(sK2, sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f306, f264])).
fof(f264, plain, m2_filter_2(sK2, sK1), inference(cnf_transformation, [], [f206])).
fof(f306, plain, ! [X0, X1] : (~ m2_filter_2(X1, X0) | m2_lattice4(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f141])).
fof(f141, plain, ! [X0] : (! [X1] : ((m2_lattice4(X1, X0) & ~ v1_xboole_0(X1)) | ~ m2_filter_2(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f140])).
fof(f140, plain, ! [X0] : (! [X1] : ((m2_lattice4(X1, X0) & ~ v1_xboole_0(X1)) | ~ m2_filter_2(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_filter_2(X1, X0) => (m2_lattice4(X1, X0) & ~ v1_xboole_0(X1)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', dt_m2_filter_2)).
fof(f805, plain, ! [X0, X1] : (~ m2_lattice4(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | (k7_filter_2(X0, X1) = X1)), inference(duplicate_literal_removal, [], [f795])).
fof(f795, plain, ! [X0, X1] : ((k7_filter_2(X0, X1) = X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ m2_lattice4(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(resolution, [], [f288, f307])).
fof(f307, plain, ! [X0, X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ m2_lattice4(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f143])).
fof(f143, plain, ! [X0] : (! [X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ m2_lattice4(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f142])).
fof(f142, plain, ! [X0] : (! [X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ m2_lattice4(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f32])).
fof(f32, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_lattice4(X1, X0) => m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', dt_m2_lattice4)).
fof(f288, plain, ! [X0, X1] : (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | (k7_filter_2(X0, X1) = X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0] : (! [X1] : ((k7_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0)))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : ((k7_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0)))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) => (k7_filter_2(X0, X1) = X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', d6_filter_2)).
fof(f939, plain, m1_filter_2(k7_filter_2(sK1, sK2), k1_lattice2(sK1)), inference(subsumption_resolution, [], [f938, f261])).
fof(f938, plain, (m1_filter_2(k7_filter_2(sK1, sK2), k1_lattice2(sK1)) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f937, f262])).
fof(f937, plain, (m1_filter_2(k7_filter_2(sK1, sK2), k1_lattice2(sK1)) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f936, f263])).
fof(f936, plain, (m1_filter_2(k7_filter_2(sK1, sK2), k1_lattice2(sK1)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f935, f264])).
fof(f935, plain, (m1_filter_2(k7_filter_2(sK1, sK2), k1_lattice2(sK1)) | ~ m2_filter_2(sK2, sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(superposition, [], [f291, f812])).
fof(f812, plain, (k7_filter_2(sK1, sK2) = k15_filter_2(sK1, sK2)), inference(subsumption_resolution, [], [f811, f261])).
fof(f811, plain, ((k7_filter_2(sK1, sK2) = k15_filter_2(sK1, sK2)) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f810, f262])).
fof(f810, plain, ((k7_filter_2(sK1, sK2) = k15_filter_2(sK1, sK2)) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f807, f263])).
fof(f807, plain, ((k7_filter_2(sK1, sK2) = k15_filter_2(sK1, sK2)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f407, f264])).
fof(f407, plain, ! [X0, X1] : (~ m2_filter_2(X1, X0) | (k7_filter_2(X0, X1) = k15_filter_2(X0, X1)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ! [X0, X1] : ((k7_filter_2(X0, X1) = k15_filter_2(X0, X1)) | ~ m2_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f182])).
fof(f182, plain, ! [X0, X1] : ((k7_filter_2(X0, X1) = k15_filter_2(X0, X1)) | (~ m2_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1] : ((m2_filter_2(X1, X0) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (k7_filter_2(X0, X1) = k15_filter_2(X0, X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', redefinition_k15_filter_2)).
fof(f291, plain, ! [X0, X1] : (m1_filter_2(k15_filter_2(X0, X1), k1_lattice2(X0)) | ~ m2_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f125])).
fof(f125, plain, ! [X0, X1] : (m1_filter_2(k15_filter_2(X0, X1), k1_lattice2(X0)) | ~ m2_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0, X1] : (m1_filter_2(k15_filter_2(X0, X1), k1_lattice2(X0)) | (~ m2_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : ((m2_filter_2(X1, X0) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m1_filter_2(k15_filter_2(X0, X1), k1_lattice2(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', dt_k15_filter_2)).
fof(f793, plain, (! [X0] : (~ m1_filter_0(X0, k1_lattice2(sK1)) | r2_hidden(k5_lattices(sK1), X0)) | ~ spl28_17), inference(avatar_component_clause, [], [f792])).
fof(f792, plain, (spl28_17 <=> ! [X0] : (r2_hidden(k5_lattices(sK1), X0) | ~ m1_filter_0(X0, k1_lattice2(sK1)))), introduced(avatar_definition, [new_symbols(naming, [spl28_17])])).
fof(f860, plain, spl28_16, inference(avatar_contradiction_clause, [], [f859])).
fof(f859, plain, ($false | spl28_16), inference(subsumption_resolution, [], [f858, f263])).
fof(f858, plain, (~ l3_lattices(sK1) | spl28_16), inference(resolution, [], [f790, f293])).
fof(f293, plain, ! [X0] : (l3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : ((l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (l3_lattices(X0) => (l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', dt_k1_lattice2)).
fof(f790, plain, (~ l3_lattices(k1_lattice2(sK1)) | spl28_16), inference(avatar_component_clause, [], [f788])).
fof(f851, plain, spl28_15, inference(avatar_contradiction_clause, [], [f850])).
fof(f850, plain, ($false | spl28_15), inference(subsumption_resolution, [], [f849, f261])).
fof(f849, plain, (v3_struct_0(sK1) | spl28_15), inference(subsumption_resolution, [], [f848, f262])).
fof(f848, plain, (~ v10_lattices(sK1) | v3_struct_0(sK1) | spl28_15), inference(subsumption_resolution, [], [f847, f263])).
fof(f847, plain, (~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | spl28_15), inference(subsumption_resolution, [], [f846, f265])).
fof(f265, plain, v13_lattices(sK1), inference(cnf_transformation, [], [f206])).
fof(f846, plain, (~ v13_lattices(sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | spl28_15), inference(resolution, [], [f786, f419])).
fof(f419, plain, ! [X0] : (v14_lattices(k1_lattice2(X0)) | ~ v13_lattices(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f260])).
fof(f260, plain, ! [X0] : (((v13_lattices(X0) | ~ v14_lattices(k1_lattice2(X0))) & (v14_lattices(k1_lattice2(X0)) | ~ v13_lattices(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f196])).
fof(f196, plain, ! [X0] : ((v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f195])).
fof(f195, plain, ! [X0] : ((v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', t63_lattice2)).
fof(f786, plain, (~ v14_lattices(k1_lattice2(sK1)) | spl28_15), inference(avatar_component_clause, [], [f784])).
fof(f784, plain, (spl28_15 <=> v14_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl28_15])])).
fof(f831, plain, spl28_14, inference(avatar_contradiction_clause, [], [f830])).
fof(f830, plain, ($false | spl28_14), inference(subsumption_resolution, [], [f829, f577])).
fof(f577, plain, sP0(sK1), inference(subsumption_resolution, [], [f576, f261])).
fof(f576, plain, (sP0(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f570, f262])).
fof(f570, plain, (sP0(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f356, f263])).
fof(f356, plain, ! [X0] : (~ l3_lattices(X0) | sP0(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f203])).
fof(f203, plain, ! [X0] : (sP0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(definition_folding, [], [f174, e202])).
fof(f202, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(usedef, [], [e202])).
fof(e202, plain, ! [X0] : (sP0(X0) <=> (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f174, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f173])).
fof(f173, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f59])).
fof(f59, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', fc6_lattice2)).
fof(f829, plain, (~ sP0(sK1) | spl28_14), inference(resolution, [], [f782, f355])).
fof(f355, plain, ! [X0] : (v10_lattices(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f229])).
fof(f229, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(nnf_transformation, [], [f202])).
fof(f782, plain, (~ v10_lattices(k1_lattice2(sK1)) | spl28_14), inference(avatar_component_clause, [], [f780])).
fof(f794, plain, (spl28_11 | ~ spl28_14 | ~ spl28_15 | ~ spl28_16 | spl28_17), inference(avatar_split_clause, [], [f771, f792, f788, f784, f780, f739])).
fof(f771, plain, ! [X0] : (r2_hidden(k5_lattices(sK1), X0) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v14_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ m1_filter_0(X0, k1_lattice2(sK1))), inference(superposition, [], [f425, f733])).
fof(f733, plain, (k5_lattices(sK1) = k6_lattices(k1_lattice2(sK1))), inference(subsumption_resolution, [], [f732, f261])).
fof(f732, plain, ((k5_lattices(sK1) = k6_lattices(k1_lattice2(sK1))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f731, f262])).
fof(f731, plain, ((k5_lattices(sK1) = k6_lattices(k1_lattice2(sK1))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f729, f263])).
fof(f729, plain, (~ l3_lattices(sK1) | (k5_lattices(sK1) = k6_lattices(k1_lattice2(sK1))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f422, f265])).
fof(f422, plain, ! [X0] : (~ v13_lattices(X0) | ~ l3_lattices(X0) | (k5_lattices(X0) = k6_lattices(k1_lattice2(X0))) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : ((k5_lattices(X0) = k6_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v13_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f198])).
fof(f198, plain, ! [X0] : ((k5_lattices(X0) = k6_lattices(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v13_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f87])).
fof(f87, plain, ! [X0] : ((l3_lattices(X0) & v13_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (k5_lattices(X0) = k6_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', t78_lattice2)).
fof(f425, plain, ! [X0, X1] : (r2_hidden(k6_lattices(X0), X1) | ~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ m1_filter_0(X1, X0)), inference(duplicate_literal_removal, [], [f413])).
fof(f413, plain, ! [X0, X1] : (r2_hidden(k6_lattices(X0), X1) | ~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ m1_filter_0(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ! [X0] : (! [X1] : (r2_hidden(k6_lattices(X0), X1) | ~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ m1_filter_0(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f186])).
fof(f186, plain, ! [X0] : (! [X1] : ((r2_hidden(k6_lattices(X0), X1) | (~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))) | ~ m1_filter_0(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_filter_0(X1, X0) => ((l3_lattices(X0) & v14_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => r2_hidden(k6_lattices(X0), X1)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT301+1.p', t12_filter_0)).
fof(f778, plain, ~ spl28_11, inference(avatar_contradiction_clause, [], [f777])).
fof(f777, plain, ($false | ~ spl28_11), inference(subsumption_resolution, [], [f773, f577])).
fof(f773, plain, (~ sP0(sK1) | ~ spl28_11), inference(resolution, [], [f741, f347])).
fof(f347, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f229])).
fof(f741, plain, (v3_struct_0(k1_lattice2(sK1)) | ~ spl28_11), inference(avatar_component_clause, [], [f739])).