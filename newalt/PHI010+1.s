fof(f82, plain, $false, inference(subsumption_resolution, [], [f81, f74])).
fof(f74, plain, sP0(sK4, sK5), inference(resolution, [], [f73, f48])).
fof(f48, plain, is_the(sK4, sK5), inference(cnf_transformation, [], [f28])).
fof(f28, plain, (~ exemplifies_property(sK5, sK6) & (sK4 = sK6) & is_the(sK4, sK5) & object(sK6) & property(sK5) & object(sK4)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6])], [f15, f27])).
fof(f27, plain, (? [X0, X1, X2] : (~ exemplifies_property(X1, X2) & (X0 = X2) & is_the(X0, X1) & object(X2) & property(X1) & object(X0)) => (~ exemplifies_property(sK5, sK6) & (sK4 = sK6) & is_the(sK4, sK5) & object(sK6) & property(sK5) & object(sK4))), introduced(choice_axiom, [])).
fof(f15, plain, ? [X0, X1, X2] : (~ exemplifies_property(X1, X2) & (X0 = X2) & is_the(X0, X1) & object(X2) & property(X1) & object(X0)), inference(flattening, [], [f14])).
fof(f14, plain, ? [X0, X1, X2] : ((~ exemplifies_property(X1, X2) & ((X0 = X2) & is_the(X0, X1))) & (object(X2) & property(X1) & object(X0))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ! [X0, X1, X2] : ((object(X2) & property(X1) & object(X0)) => (((X0 = X2) & is_the(X0, X1)) => exemplifies_property(X1, X2))), inference(rectify, [], [f6])).
fof(f6, plain, ~ ! [X0, X1, X3] : ((object(X3) & property(X1) & object(X0)) => (((X0 = X3) & is_the(X0, X1)) => exemplifies_property(X1, X3))), inference(negated_conjecture, [], [f5])).
fof(f5, plain, ~ ! [X0, X1, X3] : ((object(X3) & property(X1) & object(X0)) => (((X0 = X3) & is_the(X0, X1)) => exemplifies_property(X1, X3))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI010+1.p', lemma_1)).
fof(f73, plain, ! [X0, X1] : (~ is_the(X0, X1) | sP0(X0, X1)), inference(subsumption_resolution, [], [f72, f32])).
fof(f32, plain, ! [X0, X1] : (~ is_the(X0, X1) | property(X1)), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0, X1] : ((object(X0) & property(X1)) | ~ is_the(X0, X1)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0, X1] : (is_the(X0, X1) => (object(X0) & property(X1))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI010+1.p', description_is_property_and_described_is_object)).
fof(f72, plain, ! [X0, X1] : (~ is_the(X0, X1) | sP0(X0, X1) | ~ property(X1)), inference(subsumption_resolution, [], [f71, f33])).
fof(f33, plain, ! [X0, X1] : (~ is_the(X0, X1) | object(X0)), inference(cnf_transformation, [], [f11])).
fof(f71, plain, ! [X0, X1] : (~ is_the(X0, X1) | sP0(X0, X1) | ~ object(X0) | ~ property(X1)), inference(duplicate_literal_removal, [], [f70])).
fof(f70, plain, ! [X0, X1] : (~ is_the(X0, X1) | sP0(X0, X1) | ~ object(X0) | ~ object(X0) | ~ property(X1)), inference(resolution, [], [f51, f44])).
fof(f44, plain, ! [X2, X0, X1] : (sP1(X0, X2, X1) | ~ object(X2) | ~ object(X1) | ~ property(X0)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ! [X0, X1, X2] : (sP1(X0, X2, X1) | ~ object(X2) | ~ object(X1) | ~ property(X0)), inference(definition_folding, [], [f13, e17, e16])).
fof(f16, plain, ! [X2, X0] : (sP0(X2, X0) <=> ? [X3] : ((X2 = X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))), inference(usedef, [], [e16])).
fof(e16, plain, ! [X2, X0] : (sP0(X2, X0) <=> ? [X3] : ((X2 = X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f17, plain, ! [X0, X2, X1] : ((((X1 = X2) & is_the(X1, X0)) <=> sP0(X2, X0)) | ~ sP1(X0, X2, X1)), inference(usedef, [], [e17])).
fof(e17, plain, ! [X0, X2, X1] : (sP1(X0, X2, X1) <=> (((X1 = X2) & is_the(X1, X0)) <=> sP0(X2, X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f13, plain, ! [X0, X1, X2] : ((((X1 = X2) & is_the(X1, X0)) <=> ? [X3] : ((X2 = X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))) | ~ object(X2) | ~ object(X1) | ~ property(X0)), inference(flattening, [], [f12])).
fof(f12, plain, ! [X0, X1, X2] : ((((X1 = X2) & is_the(X1, X0)) <=> ? [X3] : ((X2 = X3) & ! [X4] : (((X3 = X4) | ~ exemplifies_property(X0, X4)) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))) | (~ object(X2) | ~ object(X1) | ~ property(X0))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0, X1, X2] : ((object(X2) & object(X1) & property(X0)) => (((X1 = X2) & is_the(X1, X0)) <=> ? [X3] : ((X2 = X3) & ! [X4] : (object(X4) => (exemplifies_property(X0, X4) => (X3 = X4))) & exemplifies_property(X0, X3) & object(X3)))), inference(rectify, [], [f4])).
fof(f4, plain, ! [X1, X0, X2] : ((object(X2) & object(X0) & property(X1)) => (((X0 = X2) & is_the(X0, X1)) <=> ? [X3] : ((X2 = X3) & ! [X4] : (object(X4) => (exemplifies_property(X1, X4) => (X3 = X4))) & exemplifies_property(X1, X3) & object(X3)))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI010+1.p', description_axiom_identity_instance)).
fof(f51, plain, ! [X2, X0] : (~ sP1(X0, X2, X2) | ~ is_the(X2, X0) | sP0(X2, X0)), inference(equality_resolution, [], [f34])).
fof(f34, plain, ! [X2, X0, X1] : (sP0(X1, X0) | ~ (X1 = X2) | ~ is_the(X2, X0) | ~ sP1(X0, X1, X2)), inference(cnf_transformation, [], [f21])).
fof(f21, plain, ! [X0, X1, X2] : (((((X1 = X2) & is_the(X2, X0)) | ~ sP0(X1, X0)) & (sP0(X1, X0) | ~ (X1 = X2) | ~ is_the(X2, X0))) | ~ sP1(X0, X1, X2)), inference(rectify, [], [f20])).
fof(f20, plain, ! [X0, X2, X1] : (((((X1 = X2) & is_the(X1, X0)) | ~ sP0(X2, X0)) & (sP0(X2, X0) | ~ (X1 = X2) | ~ is_the(X1, X0))) | ~ sP1(X0, X2, X1)), inference(flattening, [], [f19])).
fof(f19, plain, ! [X0, X2, X1] : (((((X1 = X2) & is_the(X1, X0)) | ~ sP0(X2, X0)) & (sP0(X2, X0) | (~ (X1 = X2) | ~ is_the(X1, X0)))) | ~ sP1(X0, X2, X1)), inference(nnf_transformation, [], [f17])).
fof(f81, plain, ~ sP0(sK4, sK5), inference(subsumption_resolution, [], [f79, f59])).
fof(f59, plain, ~ exemplifies_property(sK5, sK4), inference(backward_demodulation, [], [f50, f49])).
fof(f49, plain, (sK4 = sK6), inference(cnf_transformation, [], [f28])).
fof(f50, plain, ~ exemplifies_property(sK5, sK6), inference(cnf_transformation, [], [f28])).
fof(f79, plain, (exemplifies_property(sK5, sK4) | ~ sP0(sK4, sK5)), inference(superposition, [], [f38, f76])).
fof(f76, plain, (sK4 = sK3(sK4, sK5)), inference(resolution, [], [f74, f40])).
fof(f40, plain, ! [X0, X1] : (~ sP0(X0, X1) | (sK3(X0, X1) = X0)), inference(cnf_transformation, [], [f26])).
fof(f26, plain, ! [X0, X1] : ((sP0(X0, X1) | ! [X2] : (~ (X0 = X2) | (~ (sK2(X1, X2) = X2) & exemplifies_property(X1, sK2(X1, X2)) & object(sK2(X1, X2))) | ~ exemplifies_property(X1, X2) | ~ object(X2))) & (((sK3(X0, X1) = X0) & ! [X5] : ((sK3(X0, X1) = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, sK3(X0, X1)) & object(sK3(X0, X1))) | ~ sP0(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3])], [f23, f25, f24])).
fof(f24, plain, ! [X2, X1] : (? [X3] : (~ (X2 = X3) & exemplifies_property(X1, X3) & object(X3)) => (~ (sK2(X1, X2) = X2) & exemplifies_property(X1, sK2(X1, X2)) & object(sK2(X1, X2)))), introduced(choice_axiom, [])).
fof(f25, plain, ! [X1, X0] : (? [X4] : ((X0 = X4) & ! [X5] : ((X4 = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, X4) & object(X4)) => ((sK3(X0, X1) = X0) & ! [X5] : ((sK3(X0, X1) = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, sK3(X0, X1)) & object(sK3(X0, X1)))), introduced(choice_axiom, [])).
fof(f23, plain, ! [X0, X1] : ((sP0(X0, X1) | ! [X2] : (~ (X0 = X2) | ? [X3] : (~ (X2 = X3) & exemplifies_property(X1, X3) & object(X3)) | ~ exemplifies_property(X1, X2) | ~ object(X2))) & (? [X4] : ((X0 = X4) & ! [X5] : ((X4 = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, X4) & object(X4)) | ~ sP0(X0, X1))), inference(rectify, [], [f22])).
fof(f22, plain, ! [X2, X0] : ((sP0(X2, X0) | ! [X3] : (~ (X2 = X3) | ? [X4] : (~ (X3 = X4) & exemplifies_property(X0, X4) & object(X4)) | ~ exemplifies_property(X0, X3) | ~ object(X3))) & (? [X3] : ((X2 = X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3)) | ~ sP0(X2, X0))), inference(nnf_transformation, [], [f16])).
fof(f38, plain, ! [X0, X1] : (exemplifies_property(X1, sK3(X0, X1)) | ~ sP0(X0, X1)), inference(cnf_transformation, [], [f26])).