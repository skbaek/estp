fof(f108, plain, $false, inference(avatar_sat_refutation, [], [f58, f78, f84, f107])).
fof(f107, plain, (spl6_2 | ~ spl6_1 | ~ spl6_3), inference(avatar_split_clause, [], [f106, f81, f52, f56])).
fof(f56, plain, (spl6_2 <=> ! [X0] : (~ exemplifies_property(X0, sK5) | sP0(X0, sK4))), introduced(avatar_definition, [new_symbols(naming, [spl6_2])])).
fof(f52, plain, (spl6_1 <=> object(sK2(sK4, sK5))), introduced(avatar_definition, [new_symbols(naming, [spl6_1])])).
fof(f81, plain, (spl6_3 <=> exemplifies_property(sK4, sK2(sK4, sK5))), introduced(avatar_definition, [new_symbols(naming, [spl6_3])])).
fof(f106, plain, (! [X0] : (~ exemplifies_property(X0, sK5) | sP0(X0, sK4)) | (~ spl6_1 | ~ spl6_3)), inference(subsumption_resolution, [], [f105, f35])).
fof(f35, plain, object(sK5), inference(cnf_transformation, [], [f22])).
fof(f22, plain, (! [X1] : (~ is_the(X1, sK4) | ~ object(X1)) & (! [X3] : ((sK5 = X3) | ~ exemplifies_property(sK4, X3) | ~ object(X3)) & exemplifies_property(sK4, sK5) & object(sK5)) & property(sK4)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5])], [f19, f21, f20])).
fof(f20, plain, (? [X0] : (! [X1] : (~ is_the(X1, X0) | ~ object(X1)) & ? [X2] : (! [X3] : ((X2 = X3) | ~ exemplifies_property(X0, X3) | ~ object(X3)) & exemplifies_property(X0, X2) & object(X2)) & property(X0)) => (! [X1] : (~ is_the(X1, sK4) | ~ object(X1)) & ? [X2] : (! [X3] : ((X2 = X3) | ~ exemplifies_property(sK4, X3) | ~ object(X3)) & exemplifies_property(sK4, X2) & object(X2)) & property(sK4))), introduced(choice_axiom, [])).
fof(f21, plain, (? [X2] : (! [X3] : ((X2 = X3) | ~ exemplifies_property(sK4, X3) | ~ object(X3)) & exemplifies_property(sK4, X2) & object(X2)) => (! [X3] : ((sK5 = X3) | ~ exemplifies_property(sK4, X3) | ~ object(X3)) & exemplifies_property(sK4, sK5) & object(sK5))), introduced(choice_axiom, [])).
fof(f19, plain, ? [X0] : (! [X1] : (~ is_the(X1, X0) | ~ object(X1)) & ? [X2] : (! [X3] : ((X2 = X3) | ~ exemplifies_property(X0, X3) | ~ object(X3)) & exemplifies_property(X0, X2) & object(X2)) & property(X0)), inference(rectify, [], [f8])).
fof(f8, plain, ? [X0] : (! [X3] : (~ is_the(X3, X0) | ~ object(X3)) & ? [X1] : (! [X2] : ((X1 = X2) | ~ exemplifies_property(X0, X2) | ~ object(X2)) & exemplifies_property(X0, X1) & object(X1)) & property(X0)), inference(flattening, [], [f7])).
fof(f7, plain, ? [X0] : ((! [X3] : (~ is_the(X3, X0) | ~ object(X3)) & ? [X1] : (! [X2] : (((X1 = X2) | ~ exemplifies_property(X0, X2)) | ~ object(X2)) & exemplifies_property(X0, X1) & object(X1))) & property(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ~ ! [X0] : (property(X0) => (? [X1] : (! [X2] : (object(X2) => (exemplifies_property(X0, X2) => (X1 = X2))) & exemplifies_property(X0, X1) & object(X1)) => ? [X3] : (is_the(X3, X0) & object(X3)))), inference(rectify, [], [f3])).
fof(f3, plain, ~ ! [X0] : (property(X0) => (? [X3] : (! [X4] : (object(X4) => (exemplifies_property(X0, X4) => (X3 = X4))) & exemplifies_property(X0, X3) & object(X3)) => ? [X5] : (is_the(X5, X0) & object(X5)))), inference(negated_conjecture, [], [f2])).
fof(f2, plain, ~ ! [X0] : (property(X0) => (? [X3] : (! [X4] : (object(X4) => (exemplifies_property(X0, X4) => (X3 = X4))) & exemplifies_property(X0, X3) & object(X3)) => ? [X5] : (is_the(X5, X0) & object(X5)))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI009+1.p', description_theorem_1)).
fof(f105, plain, (! [X0] : (~ exemplifies_property(X0, sK5) | sP0(X0, sK4) | ~ object(sK5)) | (~ spl6_1 | ~ spl6_3)), inference(subsumption_resolution, [], [f104, f36])).
fof(f36, plain, exemplifies_property(sK4, sK5), inference(cnf_transformation, [], [f22])).
fof(f104, plain, (! [X0] : (~ exemplifies_property(X0, sK5) | sP0(X0, sK4) | ~ exemplifies_property(sK4, sK5) | ~ object(sK5)) | (~ spl6_1 | ~ spl6_3)), inference(trivial_inequality_removal, [], [f103])).
fof(f103, plain, (! [X0] : (~ (sK5 = sK5) | ~ exemplifies_property(X0, sK5) | sP0(X0, sK4) | ~ exemplifies_property(sK4, sK5) | ~ object(sK5)) | (~ spl6_1 | ~ spl6_3)), inference(superposition, [], [f32, f88])).
fof(f88, plain, ((sK5 = sK2(sK4, sK5)) | (~ spl6_1 | ~ spl6_3)), inference(subsumption_resolution, [], [f85, f54])).
fof(f54, plain, (object(sK2(sK4, sK5)) | ~ spl6_1), inference(avatar_component_clause, [], [f52])).
fof(f85, plain, ((sK5 = sK2(sK4, sK5)) | ~ object(sK2(sK4, sK5)) | ~ spl6_3), inference(resolution, [], [f83, f37])).
fof(f37, plain, ! [X3] : (~ exemplifies_property(sK4, X3) | (sK5 = X3) | ~ object(X3)), inference(cnf_transformation, [], [f22])).
fof(f83, plain, (exemplifies_property(sK4, sK2(sK4, sK5)) | ~ spl6_3), inference(avatar_component_clause, [], [f81])).
fof(f32, plain, ! [X2, X0, X1] : (~ (sK2(X1, X2) = X2) | ~ exemplifies_property(X0, X2) | sP0(X0, X1) | ~ exemplifies_property(X1, X2) | ~ object(X2)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ! [X0, X1] : ((sP0(X0, X1) | ! [X2] : (~ exemplifies_property(X0, X2) | (~ (sK2(X1, X2) = X2) & exemplifies_property(X1, sK2(X1, X2)) & object(sK2(X1, X2))) | ~ exemplifies_property(X1, X2) | ~ object(X2))) & ((exemplifies_property(X0, sK3(X0, X1)) & ! [X5] : ((sK3(X0, X1) = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, sK3(X0, X1)) & object(sK3(X0, X1))) | ~ sP0(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3])], [f15, f17, f16])).
fof(f16, plain, ! [X2, X1] : (? [X3] : (~ (X2 = X3) & exemplifies_property(X1, X3) & object(X3)) => (~ (sK2(X1, X2) = X2) & exemplifies_property(X1, sK2(X1, X2)) & object(sK2(X1, X2)))), introduced(choice_axiom, [])).
fof(f17, plain, ! [X1, X0] : (? [X4] : (exemplifies_property(X0, X4) & ! [X5] : ((X4 = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, X4) & object(X4)) => (exemplifies_property(X0, sK3(X0, X1)) & ! [X5] : ((sK3(X0, X1) = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, sK3(X0, X1)) & object(sK3(X0, X1)))), introduced(choice_axiom, [])).
fof(f15, plain, ! [X0, X1] : ((sP0(X0, X1) | ! [X2] : (~ exemplifies_property(X0, X2) | ? [X3] : (~ (X2 = X3) & exemplifies_property(X1, X3) & object(X3)) | ~ exemplifies_property(X1, X2) | ~ object(X2))) & (? [X4] : (exemplifies_property(X0, X4) & ! [X5] : ((X4 = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, X4) & object(X4)) | ~ sP0(X0, X1))), inference(rectify, [], [f14])).
fof(f14, plain, ! [X1, X0] : ((sP0(X1, X0) | ! [X3] : (~ exemplifies_property(X1, X3) | ? [X4] : (~ (X3 = X4) & exemplifies_property(X0, X4) & object(X4)) | ~ exemplifies_property(X0, X3) | ~ object(X3))) & (? [X3] : (exemplifies_property(X1, X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3)) | ~ sP0(X1, X0))), inference(nnf_transformation, [], [f9])).
fof(f9, plain, ! [X1, X0] : (sP0(X1, X0) <=> ? [X3] : (exemplifies_property(X1, X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))), inference(usedef, [], [e9])).
fof(e9, plain, ! [X1, X0] : (sP0(X1, X0) <=> ? [X3] : (exemplifies_property(X1, X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f84, plain, (spl6_3 | spl6_2), inference(avatar_split_clause, [], [f79, f56, f81])).
fof(f79, plain, ! [X0] : (~ exemplifies_property(X0, sK5) | exemplifies_property(sK4, sK2(sK4, sK5)) | sP0(X0, sK4)), inference(subsumption_resolution, [], [f62, f35])).
fof(f62, plain, ! [X0] : (~ exemplifies_property(X0, sK5) | exemplifies_property(sK4, sK2(sK4, sK5)) | sP0(X0, sK4) | ~ object(sK5)), inference(resolution, [], [f31, f36])).
fof(f31, plain, ! [X2, X0, X1] : (~ exemplifies_property(X1, X2) | ~ exemplifies_property(X0, X2) | exemplifies_property(X1, sK2(X1, X2)) | sP0(X0, X1) | ~ object(X2)), inference(cnf_transformation, [], [f18])).
fof(f78, plain, ~ spl6_2, inference(avatar_contradiction_clause, [], [f76])).
fof(f76, plain, ($false | ~ spl6_2), inference(resolution, [], [f75, f35])).
fof(f75, plain, (! [X0] : ~ object(X0) | ~ spl6_2), inference(subsumption_resolution, [], [f74, f38])).
fof(f38, plain, ! [X1] : (~ is_the(X1, sK4) | ~ object(X1)), inference(cnf_transformation, [], [f22])).
fof(f74, plain, (! [X0] : (~ object(X0) | is_the(X0, sK4)) | ~ spl6_2), inference(subsumption_resolution, [], [f73, f34])).
fof(f34, plain, property(sK4), inference(cnf_transformation, [], [f22])).
fof(f73, plain, (! [X0] : (~ property(sK4) | ~ object(X0) | is_the(X0, sK4)) | ~ spl6_2), inference(duplicate_literal_removal, [], [f69])).
fof(f69, plain, (! [X0] : (~ property(sK4) | ~ property(sK4) | ~ object(X0) | is_the(X0, sK4)) | ~ spl6_2), inference(resolution, [], [f61, f45])).
fof(f45, plain, ! [X4, X5, X3] : (~ sP0(X4, X5) | ~ property(X4) | ~ property(X5) | ~ object(X3) | is_the(X3, X5)), inference(resolution, [], [f33, f24])).
fof(f24, plain, ! [X2, X0, X1] : (~ sP1(X0, X1, X2) | ~ sP0(X1, X0) | is_the(X2, X0)), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1, X2] : ((((exemplifies_property(X1, X2) & is_the(X2, X0)) | ~ sP0(X1, X0)) & (sP0(X1, X0) | ~ exemplifies_property(X1, X2) | ~ is_the(X2, X0))) | ~ sP1(X0, X1, X2)), inference(flattening, [], [f12])).
fof(f12, plain, ! [X0, X1, X2] : ((((exemplifies_property(X1, X2) & is_the(X2, X0)) | ~ sP0(X1, X0)) & (sP0(X1, X0) | (~ exemplifies_property(X1, X2) | ~ is_the(X2, X0)))) | ~ sP1(X0, X1, X2)), inference(nnf_transformation, [], [f10])).
fof(f10, plain, ! [X0, X1, X2] : (((exemplifies_property(X1, X2) & is_the(X2, X0)) <=> sP0(X1, X0)) | ~ sP1(X0, X1, X2)), inference(usedef, [], [e10])).
fof(e10, plain, ! [X0, X1, X2] : (sP1(X0, X1, X2) <=> ((exemplifies_property(X1, X2) & is_the(X2, X0)) <=> sP0(X1, X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f33, plain, ! [X2, X0, X1] : (sP1(X0, X1, X2) | ~ object(X2) | ~ property(X1) | ~ property(X0)), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0, X1, X2] : (sP1(X0, X1, X2) | ~ object(X2) | ~ property(X1) | ~ property(X0)), inference(definition_folding, [], [f6, e10, e9])).
fof(f6, plain, ! [X0, X1, X2] : (((exemplifies_property(X1, X2) & is_the(X2, X0)) <=> ? [X3] : (exemplifies_property(X1, X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))) | ~ object(X2) | ~ property(X1) | ~ property(X0)), inference(flattening, [], [f5])).
fof(f5, plain, ! [X0, X1, X2] : (((exemplifies_property(X1, X2) & is_the(X2, X0)) <=> ? [X3] : (exemplifies_property(X1, X3) & ! [X4] : (((X3 = X4) | ~ exemplifies_property(X0, X4)) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))) | (~ object(X2) | ~ property(X1) | ~ property(X0))), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1, X2] : ((object(X2) & property(X1) & property(X0)) => ((exemplifies_property(X1, X2) & is_the(X2, X0)) <=> ? [X3] : (exemplifies_property(X1, X3) & ! [X4] : (object(X4) => (exemplifies_property(X0, X4) => (X3 = X4))) & exemplifies_property(X0, X3) & object(X3)))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI009+1.p', description_axiom_schema_instance)).
fof(f61, plain, (sP0(sK4, sK4) | ~ spl6_2), inference(resolution, [], [f57, f36])).
fof(f57, plain, (! [X0] : (~ exemplifies_property(X0, sK5) | sP0(X0, sK4)) | ~ spl6_2), inference(avatar_component_clause, [], [f56])).
fof(f58, plain, (spl6_1 | spl6_2), inference(avatar_split_clause, [], [f50, f56, f52])).
fof(f50, plain, ! [X0] : (~ exemplifies_property(X0, sK5) | object(sK2(sK4, sK5)) | sP0(X0, sK4)), inference(subsumption_resolution, [], [f47, f35])).
fof(f47, plain, ! [X0] : (~ exemplifies_property(X0, sK5) | object(sK2(sK4, sK5)) | sP0(X0, sK4) | ~ object(sK5)), inference(resolution, [], [f30, f36])).
fof(f30, plain, ! [X2, X0, X1] : (~ exemplifies_property(X1, X2) | ~ exemplifies_property(X0, X2) | object(sK2(X1, X2)) | sP0(X0, X1) | ~ object(X2)), inference(cnf_transformation, [], [f18])).