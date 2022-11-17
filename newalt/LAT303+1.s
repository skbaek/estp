fof(f72869, plain, $false, inference(avatar_sat_refutation, [], [f845, f1126, f1129, f2081, f2097, f2100, f2116, f2145, f3783, f3797, f4165, f72868])).
fof(f72868, plain, (~ spl30_14 | ~ spl30_66 | ~ spl30_253 | spl30_263), inference(avatar_contradiction_clause, [], [f72867])).
fof(f72867, plain, ($false | (~ spl30_14 | ~ spl30_66 | ~ spl30_253 | spl30_263)), inference(subsumption_resolution, [], [f72866, f268])).
fof(f268, plain, m1_subset_1(sK2, u1_struct_0(sK1)), inference(cnf_transformation, [], [f205])).
fof(f205, plain, ((~ v13_lattices(sK1) & m2_filter_2(k6_domain_1(u1_struct_0(sK1), sK2), sK1) & m1_subset_1(sK2, u1_struct_0(sK1))) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2])], [f107, f204, f203])).
fof(f203, plain, (? [X0] : (? [X1] : (~ v13_lattices(X0) & m2_filter_2(k6_domain_1(u1_struct_0(X0), X1), X0) & m1_subset_1(X1, u1_struct_0(X0))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (? [X1] : (~ v13_lattices(sK1) & m2_filter_2(k6_domain_1(u1_struct_0(sK1), X1), sK1) & m1_subset_1(X1, u1_struct_0(sK1))) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1))), introduced(choice_axiom, [])).
fof(f204, plain, (? [X1] : (~ v13_lattices(sK1) & m2_filter_2(k6_domain_1(u1_struct_0(sK1), X1), sK1) & m1_subset_1(X1, u1_struct_0(sK1))) => (~ v13_lattices(sK1) & m2_filter_2(k6_domain_1(u1_struct_0(sK1), sK2), sK1) & m1_subset_1(sK2, u1_struct_0(sK1)))), introduced(choice_axiom, [])).
fof(f107, plain, ? [X0] : (? [X1] : (~ v13_lattices(X0) & m2_filter_2(k6_domain_1(u1_struct_0(X0), X1), X0) & m1_subset_1(X1, u1_struct_0(X0))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(flattening, [], [f106])).
fof(f106, plain, ? [X0] : (? [X1] : ((~ v13_lattices(X0) & m2_filter_2(k6_domain_1(u1_struct_0(X0), X1), X0)) & m1_subset_1(X1, u1_struct_0(X0))) & (l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => (m2_filter_2(k6_domain_1(u1_struct_0(X0), X1), X0) => v13_lattices(X0)))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => (m2_filter_2(k6_domain_1(u1_struct_0(X0), X1), X0) => v13_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', t27_filter_2)).
fof(f72866, plain, (~ m1_subset_1(sK2, u1_struct_0(sK1)) | (~ spl30_14 | ~ spl30_66 | ~ spl30_253 | spl30_263)), inference(subsumption_resolution, [], [f72856, f3801])).
fof(f3801, plain, (m2_filter_2(k1_tarski(sK2), sK1) | ~ spl30_253), inference(backward_demodulation, [], [f269, f3782])).
fof(f3782, plain, ((k6_domain_1(u1_struct_0(sK1), sK2) = k1_tarski(sK2)) | ~ spl30_253), inference(avatar_component_clause, [], [f3780])).
fof(f3780, plain, (spl30_253 <=> (k6_domain_1(u1_struct_0(sK1), sK2) = k1_tarski(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl30_253])])).
fof(f269, plain, m2_filter_2(k6_domain_1(u1_struct_0(sK1), sK2), sK1), inference(cnf_transformation, [], [f205])).
fof(f72856, plain, (~ m2_filter_2(k1_tarski(sK2), sK1) | ~ m1_subset_1(sK2, u1_struct_0(sK1)) | (~ spl30_14 | ~ spl30_66 | ~ spl30_253 | spl30_263)), inference(superposition, [], [f7749, f3782])).
fof(f7749, plain, (! [X0] : (~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), X0), sK1) | ~ m1_subset_1(X0, u1_struct_0(sK1))) | (~ spl30_14 | ~ spl30_66 | spl30_263)), inference(subsumption_resolution, [], [f7748, f4125])).
fof(f4125, plain, (~ v14_lattices(k1_lattice2(sK1)) | spl30_263), inference(avatar_component_clause, [], [f4124])).
fof(f4124, plain, (spl30_263 <=> v14_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_263])])).
fof(f7748, plain, (! [X0] : (~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), X0), sK1) | v14_lattices(k1_lattice2(sK1)) | ~ m1_subset_1(X0, u1_struct_0(sK1))) | (~ spl30_14 | ~ spl30_66)), inference(subsumption_resolution, [], [f7747, f814])).
fof(f814, plain, (v10_lattices(k1_lattice2(sK1)) | ~ spl30_14), inference(avatar_component_clause, [], [f813])).
fof(f813, plain, (spl30_14 <=> v10_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_14])])).
fof(f7747, plain, (! [X0] : (~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), X0), sK1) | ~ v10_lattices(k1_lattice2(sK1)) | v14_lattices(k1_lattice2(sK1)) | ~ m1_subset_1(X0, u1_struct_0(sK1))) | ~ spl30_66), inference(subsumption_resolution, [], [f7746, f267])).
fof(f267, plain, l3_lattices(sK1), inference(cnf_transformation, [], [f205])).
fof(f7746, plain, (! [X0] : (~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), X0), sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(k1_lattice2(sK1)) | v14_lattices(k1_lattice2(sK1)) | ~ m1_subset_1(X0, u1_struct_0(sK1))) | ~ spl30_66), inference(subsumption_resolution, [], [f7745, f265])).
fof(f265, plain, ~ v3_struct_0(sK1), inference(cnf_transformation, [], [f205])).
fof(f7745, plain, (! [X0] : (~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), X0), sK1) | v3_struct_0(sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(k1_lattice2(sK1)) | v14_lattices(k1_lattice2(sK1)) | ~ m1_subset_1(X0, u1_struct_0(sK1))) | ~ spl30_66), inference(subsumption_resolution, [], [f7737, f266])).
fof(f266, plain, v10_lattices(sK1), inference(cnf_transformation, [], [f205])).
fof(f7737, plain, (! [X0] : (~ m2_filter_2(k6_domain_1(u1_struct_0(sK1), X0), sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(k1_lattice2(sK1)) | v14_lattices(k1_lattice2(sK1)) | ~ m1_subset_1(X0, u1_struct_0(sK1))) | ~ spl30_66), inference(superposition, [], [f1653, f3974])).
fof(f3974, plain, ((u1_struct_0(sK1) = u1_struct_0(k1_lattice2(sK1))) | ~ spl30_66), inference(trivial_inequality_removal, [], [f3957])).
fof(f3957, plain, (~ (k1_lattice2(sK1) = k1_lattice2(sK1)) | (u1_struct_0(sK1) = u1_struct_0(k1_lattice2(sK1))) | ~ spl30_66), inference(superposition, [], [f1125, f1534])).
fof(f1534, plain, (k1_lattice2(sK1) = g3_lattices(u1_struct_0(k1_lattice2(sK1)), u2_lattices(k1_lattice2(sK1)), u1_lattices(k1_lattice2(sK1)))), inference(resolution, [], [f751, f267])).
fof(f751, plain, ! [X1] : (~ l3_lattices(X1) | (k1_lattice2(X1) = g3_lattices(u1_struct_0(k1_lattice2(X1)), u2_lattices(k1_lattice2(X1)), u1_lattices(k1_lattice2(X1))))), inference(subsumption_resolution, [], [f746, f297])).
fof(f297, plain, ! [X0] : (l3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : ((l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (l3_lattices(X0) => (l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', dt_k1_lattice2)).
fof(f746, plain, ! [X1] : ((k1_lattice2(X1) = g3_lattices(u1_struct_0(k1_lattice2(X1)), u2_lattices(k1_lattice2(X1)), u1_lattices(k1_lattice2(X1)))) | ~ l3_lattices(k1_lattice2(X1)) | ~ l3_lattices(X1)), inference(resolution, [], [f271, f296])).
fof(f296, plain, ! [X0] : (v3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f129])).
fof(f271, plain, ! [X0] : (~ v3_lattices(X0) | (g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0] : ((g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ v3_lattices(X0) | ~ l3_lattices(X0)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0] : (((g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ v3_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (l3_lattices(X0) => (v3_lattices(X0) => (g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', abstractness_v3_lattices)).
fof(f1125, plain, (! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15)) | ~ spl30_66), inference(avatar_component_clause, [], [f1124])).
fof(f1124, plain, (spl30_66 <=> ! [X16, X15, X17] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15))), introduced(avatar_definition, [new_symbols(naming, [spl30_66])])).
fof(f1653, plain, ! [X8, X7] : (~ m2_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(X7)), X8), X7) | ~ v10_lattices(X7) | v3_struct_0(X7) | ~ l3_lattices(X7) | ~ v10_lattices(k1_lattice2(X7)) | v14_lattices(k1_lattice2(X7)) | ~ m1_subset_1(X8, u1_struct_0(k1_lattice2(X7)))), inference(subsumption_resolution, [], [f1652, f329])).
fof(f329, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f157])).
fof(f157, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f156])).
fof(f156, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f48])).
fof(f48, plain, ! [X0] : ((l3_lattices(X0) & ~ v3_struct_0(X0)) => (v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', fc1_lattice2)).
fof(f1652, plain, ! [X8, X7] : (~ l3_lattices(X7) | ~ v10_lattices(X7) | v3_struct_0(X7) | ~ m2_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(X7)), X8), X7) | ~ v10_lattices(k1_lattice2(X7)) | v14_lattices(k1_lattice2(X7)) | ~ m1_subset_1(X8, u1_struct_0(k1_lattice2(X7))) | v3_struct_0(k1_lattice2(X7))), inference(subsumption_resolution, [], [f1646, f297])).
fof(f1646, plain, ! [X8, X7] : (~ l3_lattices(X7) | ~ v10_lattices(X7) | v3_struct_0(X7) | ~ m2_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(X7)), X8), X7) | ~ v10_lattices(k1_lattice2(X7)) | v14_lattices(k1_lattice2(X7)) | ~ m1_subset_1(X8, u1_struct_0(k1_lattice2(X7))) | ~ l3_lattices(k1_lattice2(X7)) | v3_struct_0(k1_lattice2(X7))), inference(duplicate_literal_removal, [], [f1645])).
fof(f1645, plain, ! [X8, X7] : (~ l3_lattices(X7) | ~ v10_lattices(X7) | v3_struct_0(X7) | ~ m2_filter_2(k6_domain_1(u1_struct_0(k1_lattice2(X7)), X8), X7) | ~ v10_lattices(k1_lattice2(X7)) | v14_lattices(k1_lattice2(X7)) | ~ m1_subset_1(X8, u1_struct_0(k1_lattice2(X7))) | ~ l3_lattices(k1_lattice2(X7)) | ~ v10_lattices(k1_lattice2(X7)) | v3_struct_0(k1_lattice2(X7))), inference(resolution, [], [f766, f422])).
fof(f422, plain, ! [X0, X1] : (~ m1_filter_0(k6_domain_1(u1_struct_0(X0), X1), X0) | v14_lattices(X0) | ~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f186])).
fof(f186, plain, ! [X0] : (! [X1] : (v14_lattices(X0) | ~ m1_filter_0(k6_domain_1(u1_struct_0(X0), X1), X0) | ~ m1_subset_1(X1, u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f185])).
fof(f185, plain, ! [X0] : (! [X1] : ((v14_lattices(X0) | ~ m1_filter_0(k6_domain_1(u1_struct_0(X0), X1), X0)) | ~ m1_subset_1(X1, u1_struct_0(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f82])).
fof(f82, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => (m1_filter_0(k6_domain_1(u1_struct_0(X0), X1), X0) => v14_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', t14_filter_0)).
fof(f766, plain, ! [X0, X1] : (m1_filter_0(X0, k1_lattice2(X1)) | ~ l3_lattices(X1) | ~ v10_lattices(X1) | v3_struct_0(X1) | ~ m2_filter_2(X0, X1) | ~ v10_lattices(k1_lattice2(X1))), inference(subsumption_resolution, [], [f765, f329])).
fof(f765, plain, ! [X0, X1] : (~ m2_filter_2(X0, X1) | ~ l3_lattices(X1) | ~ v10_lattices(X1) | v3_struct_0(X1) | m1_filter_0(X0, k1_lattice2(X1)) | ~ v10_lattices(k1_lattice2(X1)) | v3_struct_0(k1_lattice2(X1))), inference(subsumption_resolution, [], [f762, f297])).
fof(f762, plain, ! [X0, X1] : (~ m2_filter_2(X0, X1) | ~ l3_lattices(X1) | ~ v10_lattices(X1) | v3_struct_0(X1) | m1_filter_0(X0, k1_lattice2(X1)) | ~ l3_lattices(k1_lattice2(X1)) | ~ v10_lattices(k1_lattice2(X1)) | v3_struct_0(k1_lattice2(X1))), inference(resolution, [], [f424, f417])).
fof(f417, plain, ! [X0, X1] : (~ m1_filter_2(X1, X0) | m1_filter_0(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f261])).
fof(f261, plain, ! [X0] : (! [X1] : ((m1_filter_2(X1, X0) | ~ m1_filter_0(X1, X0)) & (m1_filter_0(X1, X0) | ~ m1_filter_2(X1, X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f184])).
fof(f184, plain, ! [X0] : (! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f183])).
fof(f183, plain, ! [X0] : (! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', redefinition_m1_filter_2)).
fof(f424, plain, ! [X0, X1] : (m1_filter_2(X1, k1_lattice2(X0)) | ~ m2_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f263])).
fof(f263, plain, ! [X0] : (! [X1] : ((m2_filter_2(X1, X0) | ~ m1_filter_2(X1, k1_lattice2(X0))) & (m1_filter_2(X1, k1_lattice2(X0)) | ~ m2_filter_2(X1, X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f189])).
fof(f189, plain, ! [X0] : (! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f188])).
fof(f188, plain, ! [X0] : (! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', t21_filter_2)).
fof(f4165, plain, ~ spl30_263, inference(avatar_contradiction_clause, [], [f4164])).
fof(f4164, plain, ($false | ~ spl30_263), inference(subsumption_resolution, [], [f4163, f265])).
fof(f4163, plain, (v3_struct_0(sK1) | ~ spl30_263), inference(subsumption_resolution, [], [f4162, f266])).
fof(f4162, plain, (~ v10_lattices(sK1) | v3_struct_0(sK1) | ~ spl30_263), inference(subsumption_resolution, [], [f4161, f267])).
fof(f4161, plain, (~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | ~ spl30_263), inference(subsumption_resolution, [], [f4159, f270])).
fof(f270, plain, ~ v13_lattices(sK1), inference(cnf_transformation, [], [f205])).
fof(f4159, plain, (v13_lattices(sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | ~ spl30_263), inference(resolution, [], [f4126, f431])).
fof(f431, plain, ! [X0] : (~ v14_lattices(k1_lattice2(X0)) | v13_lattices(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f264])).
fof(f264, plain, ! [X0] : (((v13_lattices(X0) | ~ v14_lattices(k1_lattice2(X0))) & (v14_lattices(k1_lattice2(X0)) | ~ v13_lattices(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f197])).
fof(f197, plain, ! [X0] : ((v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f196])).
fof(f196, plain, ! [X0] : ((v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f89])).
fof(f89, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v13_lattices(X0) <=> v14_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', t63_lattice2)).
fof(f4126, plain, (v14_lattices(k1_lattice2(sK1)) | ~ spl30_263), inference(avatar_component_clause, [], [f4124])).
fof(f3797, plain, ~ spl30_10, inference(avatar_contradiction_clause, [], [f3796])).
fof(f3796, plain, ($false | ~ spl30_10), inference(subsumption_resolution, [], [f3795, f265])).
fof(f3795, plain, (v3_struct_0(sK1) | ~ spl30_10), inference(subsumption_resolution, [], [f3789, f451])).
fof(f451, plain, l1_struct_0(sK1), inference(resolution, [], [f445, f300])).
fof(f300, plain, ! [X0] : (~ l1_lattices(X0) | l1_struct_0(X0)), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ! [X0] : (l1_struct_0(X0) | ~ l1_lattices(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (l1_lattices(X0) => l1_struct_0(X0)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', dt_l1_lattices)).
fof(f445, plain, l1_lattices(sK1), inference(resolution, [], [f302, f267])).
fof(f302, plain, ! [X0] : (~ l3_lattices(X0) | l1_lattices(X0)), inference(cnf_transformation, [], [f136])).
fof(f136, plain, ! [X0] : ((l2_lattices(X0) & l1_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (l3_lattices(X0) => (l2_lattices(X0) & l1_lattices(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', dt_l3_lattices)).
fof(f3789, plain, (~ l1_struct_0(sK1) | v3_struct_0(sK1) | ~ spl30_10), inference(resolution, [], [f614, f331])).
fof(f331, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | ~ l1_struct_0(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f159])).
fof(f159, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | ~ l1_struct_0(X0) | v3_struct_0(X0)), inference(flattening, [], [f158])).
fof(f158, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | (~ l1_struct_0(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : ((l1_struct_0(X0) & ~ v3_struct_0(X0)) => ~ v1_xboole_0(u1_struct_0(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', fc1_struct_0)).
fof(f614, plain, (v1_xboole_0(u1_struct_0(sK1)) | ~ spl30_10), inference(avatar_component_clause, [], [f612])).
fof(f612, plain, (spl30_10 <=> v1_xboole_0(u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_10])])).
fof(f3783, plain, (spl30_10 | spl30_253), inference(avatar_split_clause, [], [f684, f3780, f612])).
fof(f684, plain, ((k6_domain_1(u1_struct_0(sK1), sK2) = k1_tarski(sK2)) | v1_xboole_0(u1_struct_0(sK1))), inference(resolution, [], [f416, f268])).
fof(f416, plain, ! [X0, X1] : (~ m1_subset_1(X1, X0) | (k6_domain_1(X0, X1) = k1_tarski(X1)) | v1_xboole_0(X0)), inference(cnf_transformation, [], [f182])).
fof(f182, plain, ! [X0, X1] : ((k6_domain_1(X0, X1) = k1_tarski(X1)) | ~ m1_subset_1(X1, X0) | v1_xboole_0(X0)), inference(flattening, [], [f181])).
fof(f181, plain, ! [X0, X1] : ((k6_domain_1(X0, X1) = k1_tarski(X1)) | (~ m1_subset_1(X1, X0) | v1_xboole_0(X0))), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1] : ((m1_subset_1(X1, X0) & ~ v1_xboole_0(X0)) => (k6_domain_1(X0, X1) = k1_tarski(X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', redefinition_k6_domain_1)).
fof(f2145, plain, spl30_63, inference(avatar_contradiction_clause, [], [f2144])).
fof(f2144, plain, ($false | spl30_63), inference(subsumption_resolution, [], [f2143, f452])).
fof(f452, plain, l2_lattices(sK1), inference(resolution, [], [f303, f267])).
fof(f303, plain, ! [X0] : (~ l3_lattices(X0) | l2_lattices(X0)), inference(cnf_transformation, [], [f136])).
fof(f2143, plain, (~ l2_lattices(sK1) | spl30_63), inference(resolution, [], [f1114, f737])).
fof(f737, plain, ! [X1] : (m1_relset_1(u2_lattices(X1), k2_zfmisc_1(u1_struct_0(X1), u1_struct_0(X1)), u1_struct_0(X1)) | ~ l2_lattices(X1)), inference(resolution, [], [f317, f419])).
fof(f419, plain, ! [X2, X0, X1] : (~ m2_relset_1(X2, X0, X1) | m1_relset_1(X2, X0, X1)), inference(cnf_transformation, [], [f262])).
fof(f262, plain, ! [X0, X1, X2] : ((m2_relset_1(X2, X0, X1) | ~ m1_relset_1(X2, X0, X1)) & (m1_relset_1(X2, X0, X1) | ~ m2_relset_1(X2, X0, X1))), inference(nnf_transformation, [], [f80])).
fof(f80, plain, ! [X0, X1, X2] : (m2_relset_1(X2, X0, X1) <=> m1_relset_1(X2, X0, X1)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', redefinition_m2_relset_1)).
fof(f317, plain, ! [X0] : (m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : ((m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u2_lattices(X0))) | ~ l2_lattices(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (l2_lattices(X0) => (m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u2_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', dt_u2_lattices)).
fof(f1114, plain, (~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl30_63), inference(avatar_component_clause, [], [f1112])).
fof(f1112, plain, (spl30_63 <=> m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_63])])).
fof(f2116, plain, spl30_62, inference(avatar_contradiction_clause, [], [f2115])).
fof(f2115, plain, ($false | spl30_62), inference(subsumption_resolution, [], [f2114, f452])).
fof(f2114, plain, (~ l2_lattices(sK1) | spl30_62), inference(resolution, [], [f1110, f316])).
fof(f316, plain, ! [X0] : (v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f147])).
fof(f1110, plain, (~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl30_62), inference(avatar_component_clause, [], [f1108])).
fof(f1108, plain, (spl30_62 <=> v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_62])])).
fof(f2100, plain, spl30_61, inference(avatar_contradiction_clause, [], [f2099])).
fof(f2099, plain, ($false | spl30_61), inference(subsumption_resolution, [], [f2098, f452])).
fof(f2098, plain, (~ l2_lattices(sK1) | spl30_61), inference(resolution, [], [f1106, f315])).
fof(f315, plain, ! [X0] : (v1_funct_1(u2_lattices(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f147])).
fof(f1106, plain, (~ v1_funct_1(u2_lattices(sK1)) | spl30_61), inference(avatar_component_clause, [], [f1104])).
fof(f1104, plain, (spl30_61 <=> v1_funct_1(u2_lattices(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_61])])).
fof(f2097, plain, spl30_60, inference(avatar_contradiction_clause, [], [f2096])).
fof(f2096, plain, ($false | spl30_60), inference(subsumption_resolution, [], [f2095, f445])).
fof(f2095, plain, (~ l1_lattices(sK1) | spl30_60), inference(resolution, [], [f1102, f732])).
fof(f732, plain, ! [X1] : (m1_relset_1(u1_lattices(X1), k2_zfmisc_1(u1_struct_0(X1), u1_struct_0(X1)), u1_struct_0(X1)) | ~ l1_lattices(X1)), inference(resolution, [], [f314, f419])).
fof(f314, plain, ! [X0] : (m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f146])).
fof(f146, plain, ! [X0] : ((m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u1_lattices(X0))) | ~ l1_lattices(X0)), inference(ennf_transformation, [], [f34])).
fof(f34, plain, ! [X0] : (l1_lattices(X0) => (m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u1_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', dt_u1_lattices)).
fof(f1102, plain, (~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl30_60), inference(avatar_component_clause, [], [f1100])).
fof(f1100, plain, (spl30_60 <=> m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_60])])).
fof(f2081, plain, spl30_59, inference(avatar_contradiction_clause, [], [f2080])).
fof(f2080, plain, ($false | spl30_59), inference(subsumption_resolution, [], [f2079, f445])).
fof(f2079, plain, (~ l1_lattices(sK1) | spl30_59), inference(resolution, [], [f1098, f313])).
fof(f313, plain, ! [X0] : (v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f146])).
fof(f1098, plain, (~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl30_59), inference(avatar_component_clause, [], [f1096])).
fof(f1096, plain, (spl30_59 <=> v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_59])])).
fof(f1129, plain, spl30_58, inference(avatar_contradiction_clause, [], [f1128])).
fof(f1128, plain, ($false | spl30_58), inference(subsumption_resolution, [], [f1127, f445])).
fof(f1127, plain, (~ l1_lattices(sK1) | spl30_58), inference(resolution, [], [f1094, f312])).
fof(f312, plain, ! [X0] : (v1_funct_1(u1_lattices(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f146])).
fof(f1094, plain, (~ v1_funct_1(u1_lattices(sK1)) | spl30_58), inference(avatar_component_clause, [], [f1092])).
fof(f1092, plain, (spl30_58 <=> v1_funct_1(u1_lattices(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_58])])).
fof(f1126, plain, (~ spl30_58 | ~ spl30_59 | ~ spl30_60 | ~ spl30_61 | ~ spl30_62 | ~ spl30_63 | spl30_66), inference(avatar_split_clause, [], [f1090, f1124, f1112, f1108, f1104, f1100, f1096, f1092])).
fof(f1090, plain, ! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15) | ~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u2_lattices(sK1)) | ~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u1_lattices(sK1))), inference(superposition, [], [f362, f708])).
fof(f708, plain, (k1_lattice2(sK1) = g3_lattices(u1_struct_0(sK1), u1_lattices(sK1), u2_lattices(sK1))), inference(resolution, [], [f292, f267])).
fof(f292, plain, ! [X0] : (~ l3_lattices(X0) | (k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0)))), inference(cnf_transformation, [], [f124])).
fof(f124, plain, ! [X0] : ((k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (l3_lattices(X0) => (k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', d2_lattice2)).
fof(f362, plain, ! [X4, X2, X0, X5, X3, X1] : (~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5)) | (X0 = X3) | ~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0, X1, X2] : (! [X3, X4, X5] : (((X2 = X5) & (X1 = X4) & (X0 = X3)) | ~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5))) | ~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1)), inference(flattening, [], [f174])).
fof(f174, plain, ! [X0, X1, X2] : (! [X3, X4, X5] : (((X2 = X5) & (X1 = X4) & (X0 = X3)) | ~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5))) | (~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1))), inference(ennf_transformation, [], [f61])).
fof(f61, plain, ! [X0, X1, X2] : ((m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) & v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) & v1_funct_1(X2) & m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) & v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) & v1_funct_1(X1)) => ! [X3, X4, X5] : ((g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5)) => ((X2 = X5) & (X1 = X4) & (X0 = X3)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', free_g3_lattices)).
fof(f845, plain, spl30_14, inference(avatar_contradiction_clause, [], [f844])).
fof(f844, plain, ($false | spl30_14), inference(subsumption_resolution, [], [f843, f593])).
fof(f593, plain, sP0(sK1), inference(subsumption_resolution, [], [f592, f265])).
fof(f592, plain, (sP0(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f586, f266])).
fof(f586, plain, (sP0(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f361, f267])).
fof(f361, plain, ! [X0] : (~ l3_lattices(X0) | sP0(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : (sP0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(definition_folding, [], [f173, e201])).
fof(f201, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(usedef, [], [e201])).
fof(e201, plain, ! [X0] : (sP0(X0) <=> (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f173, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f172])).
fof(f172, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f60])).
fof(f60, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT303+1.p', fc6_lattice2)).
fof(f843, plain, (~ sP0(sK1) | spl30_14), inference(resolution, [], [f815, f360])).
fof(f360, plain, ! [X0] : (v10_lattices(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(nnf_transformation, [], [f201])).
fof(f815, plain, (~ v10_lattices(k1_lattice2(sK1)) | spl30_14), inference(avatar_component_clause, [], [f813])).