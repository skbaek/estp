fof(f221, plain, $false, inference(subsumption_resolution, [], [f220, f179])).
fof(f179, plain, exemplifies_property(none_greater, god), inference(subsumption_resolution, [], [f176, f169])).
fof(f169, plain, sP2(god, none_greater), inference(resolution, [], [f168, f99])).
fof(f99, plain, is_the(god, none_greater), inference(cnf_transformation, [], [f10])).
fof(f10, plain, is_the(god, none_greater), file('/home/ubuntu/library/tptp/Problems/PHI/PHI015+1.p', definition_god)).
fof(f168, plain, ! [X0, X1] : (~ is_the(X0, X1) | sP2(X0, X1)), inference(subsumption_resolution, [], [f167, f64])).
fof(f64, plain, ! [X0, X1] : (~ is_the(X0, X1) | property(X1)), inference(cnf_transformation, [], [f21])).
fof(f21, plain, ! [X0, X1] : ((object(X0) & property(X1)) | ~ is_the(X0, X1)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0, X1] : (is_the(X0, X1) => (object(X0) & property(X1))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI015+1.p', description_is_property_and_described_is_object)).
fof(f167, plain, ! [X0, X1] : (~ is_the(X0, X1) | sP2(X0, X1) | ~ property(X1)), inference(subsumption_resolution, [], [f166, f65])).
fof(f65, plain, ! [X0, X1] : (~ is_the(X0, X1) | object(X0)), inference(cnf_transformation, [], [f21])).
fof(f166, plain, ! [X0, X1] : (~ is_the(X0, X1) | sP2(X0, X1) | ~ object(X0) | ~ property(X1)), inference(duplicate_literal_removal, [], [f165])).
fof(f165, plain, ! [X0, X1] : (~ is_the(X0, X1) | sP2(X0, X1) | ~ object(X0) | ~ object(X0) | ~ property(X1)), inference(resolution, [], [f101, f87])).
fof(f87, plain, ! [X2, X0, X1] : (sP3(X0, X2, X1) | ~ object(X2) | ~ object(X1) | ~ property(X0)), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ! [X0, X1, X2] : (sP3(X0, X2, X1) | ~ object(X2) | ~ object(X1) | ~ property(X0)), inference(definition_folding, [], [f25, e35, e34])).
fof(f34, plain, ! [X2, X0] : (sP2(X2, X0) <=> ? [X3] : ((X2 = X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))), inference(usedef, [], [e34])).
fof(e34, plain, ! [X2, X0] : (sP2(X2, X0) <=> ? [X3] : ((X2 = X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f35, plain, ! [X0, X2, X1] : ((((X1 = X2) & is_the(X1, X0)) <=> sP2(X2, X0)) | ~ sP3(X0, X2, X1)), inference(usedef, [], [e35])).
fof(e35, plain, ! [X0, X2, X1] : (sP3(X0, X2, X1) <=> (((X1 = X2) & is_the(X1, X0)) <=> sP2(X2, X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, ! [X0, X1, X2] : ((((X1 = X2) & is_the(X1, X0)) <=> ? [X3] : ((X2 = X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))) | ~ object(X2) | ~ object(X1) | ~ property(X0)), inference(flattening, [], [f24])).
fof(f24, plain, ! [X0, X1, X2] : ((((X1 = X2) & is_the(X1, X0)) <=> ? [X3] : ((X2 = X3) & ! [X4] : (((X3 = X4) | ~ exemplifies_property(X0, X4)) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3))) | (~ object(X2) | ~ object(X1) | ~ property(X0))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ! [X0, X1, X2] : ((object(X2) & object(X1) & property(X0)) => (((X1 = X2) & is_the(X1, X0)) <=> ? [X3] : ((X2 = X3) & ! [X4] : (object(X4) => (exemplifies_property(X0, X4) => (X3 = X4))) & exemplifies_property(X0, X3) & object(X3)))), inference(rectify, [], [f5])).
fof(f5, plain, ! [X1, X0, X5] : ((object(X5) & object(X0) & property(X1)) => (((X0 = X5) & is_the(X0, X1)) <=> ? [X3] : ((X3 = X5) & ! [X4] : (object(X4) => (exemplifies_property(X1, X4) => (X3 = X4))) & exemplifies_property(X1, X3) & object(X3)))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI015+1.p', description_axiom_identity_instance)).
fof(f101, plain, ! [X2, X0] : (~ sP3(X0, X2, X2) | ~ is_the(X2, X0) | sP2(X2, X0)), inference(equality_resolution, [], [f77])).
fof(f77, plain, ! [X2, X0, X1] : (sP2(X1, X0) | ~ (X1 = X2) | ~ is_the(X2, X0) | ~ sP3(X0, X1, X2)), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ! [X0, X1, X2] : (((((X1 = X2) & is_the(X2, X0)) | ~ sP2(X1, X0)) & (sP2(X1, X0) | ~ (X1 = X2) | ~ is_the(X2, X0))) | ~ sP3(X0, X1, X2)), inference(rectify, [], [f45])).
fof(f45, plain, ! [X0, X2, X1] : (((((X1 = X2) & is_the(X1, X0)) | ~ sP2(X2, X0)) & (sP2(X2, X0) | ~ (X1 = X2) | ~ is_the(X1, X0))) | ~ sP3(X0, X2, X1)), inference(flattening, [], [f44])).
fof(f44, plain, ! [X0, X2, X1] : (((((X1 = X2) & is_the(X1, X0)) | ~ sP2(X2, X0)) & (sP2(X2, X0) | (~ (X1 = X2) | ~ is_the(X1, X0)))) | ~ sP3(X0, X2, X1)), inference(nnf_transformation, [], [f35])).
fof(f176, plain, (exemplifies_property(none_greater, god) | ~ sP2(god, none_greater)), inference(superposition, [], [f81, f171])).
fof(f171, plain, (god = sK7(god, none_greater)), inference(resolution, [], [f169, f83])).
fof(f83, plain, ! [X0, X1] : (~ sP2(X0, X1) | (sK7(X0, X1) = X0)), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ! [X0, X1] : ((sP2(X0, X1) | ! [X2] : (~ (X0 = X2) | (~ (sK6(X1, X2) = X2) & exemplifies_property(X1, sK6(X1, X2)) & object(sK6(X1, X2))) | ~ exemplifies_property(X1, X2) | ~ object(X2))) & (((sK7(X0, X1) = X0) & ! [X5] : ((sK7(X0, X1) = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, sK7(X0, X1)) & object(sK7(X0, X1))) | ~ sP2(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7])], [f48, f50, f49])).
fof(f49, plain, ! [X2, X1] : (? [X3] : (~ (X2 = X3) & exemplifies_property(X1, X3) & object(X3)) => (~ (sK6(X1, X2) = X2) & exemplifies_property(X1, sK6(X1, X2)) & object(sK6(X1, X2)))), introduced(choice_axiom, [])).
fof(f50, plain, ! [X1, X0] : (? [X4] : ((X0 = X4) & ! [X5] : ((X4 = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, X4) & object(X4)) => ((sK7(X0, X1) = X0) & ! [X5] : ((sK7(X0, X1) = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, sK7(X0, X1)) & object(sK7(X0, X1)))), introduced(choice_axiom, [])).
fof(f48, plain, ! [X0, X1] : ((sP2(X0, X1) | ! [X2] : (~ (X0 = X2) | ? [X3] : (~ (X2 = X3) & exemplifies_property(X1, X3) & object(X3)) | ~ exemplifies_property(X1, X2) | ~ object(X2))) & (? [X4] : ((X0 = X4) & ! [X5] : ((X4 = X5) | ~ exemplifies_property(X1, X5) | ~ object(X5)) & exemplifies_property(X1, X4) & object(X4)) | ~ sP2(X0, X1))), inference(rectify, [], [f47])).
fof(f47, plain, ! [X2, X0] : ((sP2(X2, X0) | ! [X3] : (~ (X2 = X3) | ? [X4] : (~ (X3 = X4) & exemplifies_property(X0, X4) & object(X4)) | ~ exemplifies_property(X0, X3) | ~ object(X3))) & (? [X3] : ((X2 = X3) & ! [X4] : ((X3 = X4) | ~ exemplifies_property(X0, X4) | ~ object(X4)) & exemplifies_property(X0, X3) & object(X3)) | ~ sP2(X2, X0))), inference(nnf_transformation, [], [f34])).
fof(f81, plain, ! [X0, X1] : (exemplifies_property(X1, sK7(X0, X1)) | ~ sP2(X0, X1)), inference(cnf_transformation, [], [f51])).
fof(f220, plain, ~ exemplifies_property(none_greater, god), inference(subsumption_resolution, [], [f219, f178])).
fof(f178, plain, exemplifies_property(conceivable, god), inference(subsumption_resolution, [], [f175, f169])).
fof(f175, plain, (exemplifies_property(conceivable, god) | ~ sP2(god, none_greater)), inference(superposition, [], [f139, f171])).
fof(f139, plain, ! [X4] : (exemplifies_property(conceivable, sK7(X4, none_greater)) | ~ sP2(X4, none_greater)), inference(resolution, [], [f81, f118])).
fof(f118, plain, ! [X0] : (~ exemplifies_property(none_greater, X0) | exemplifies_property(conceivable, X0)), inference(subsumption_resolution, [], [f89, f63])).
fof(f63, plain, ! [X0, X1] : (~ exemplifies_property(X1, X0) | object(X0)), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ! [X0, X1] : ((object(X0) & property(X1)) | ~ exemplifies_property(X1, X0)), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ! [X0, X1] : (exemplifies_property(X1, X0) => (object(X0) & property(X1))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI015+1.p', exemplifier_is_object_and_exemplified_is_property)).
fof(f89, plain, ! [X0] : (exemplifies_property(conceivable, X0) | ~ exemplifies_property(none_greater, X0) | ~ object(X0)), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ! [X0] : (((exemplifies_property(none_greater, X0) | (exemplifies_property(conceivable, sK8(X0)) & exemplifies_relation(greater_than, sK8(X0), X0) & object(sK8(X0))) | ~ exemplifies_property(conceivable, X0)) & ((! [X2] : (~ exemplifies_property(conceivable, X2) | ~ exemplifies_relation(greater_than, X2, X0) | ~ object(X2)) & exemplifies_property(conceivable, X0)) | ~ exemplifies_property(none_greater, X0))) | ~ object(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f54, f55])).
fof(f55, plain, ! [X0] : (? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)) => (exemplifies_property(conceivable, sK8(X0)) & exemplifies_relation(greater_than, sK8(X0), X0) & object(sK8(X0)))), introduced(choice_axiom, [])).
fof(f54, plain, ! [X0] : (((exemplifies_property(none_greater, X0) | ? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)) | ~ exemplifies_property(conceivable, X0)) & ((! [X2] : (~ exemplifies_property(conceivable, X2) | ~ exemplifies_relation(greater_than, X2, X0) | ~ object(X2)) & exemplifies_property(conceivable, X0)) | ~ exemplifies_property(none_greater, X0))) | ~ object(X0)), inference(rectify, [], [f53])).
fof(f53, plain, ! [X0] : (((exemplifies_property(none_greater, X0) | ? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)) | ~ exemplifies_property(conceivable, X0)) & ((! [X1] : (~ exemplifies_property(conceivable, X1) | ~ exemplifies_relation(greater_than, X1, X0) | ~ object(X1)) & exemplifies_property(conceivable, X0)) | ~ exemplifies_property(none_greater, X0))) | ~ object(X0)), inference(flattening, [], [f52])).
fof(f52, plain, ! [X0] : (((exemplifies_property(none_greater, X0) | (? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)) | ~ exemplifies_property(conceivable, X0))) & ((! [X1] : (~ exemplifies_property(conceivable, X1) | ~ exemplifies_relation(greater_than, X1, X0) | ~ object(X1)) & exemplifies_property(conceivable, X0)) | ~ exemplifies_property(none_greater, X0))) | ~ object(X0)), inference(nnf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : ((exemplifies_property(none_greater, X0) <=> (! [X1] : (~ exemplifies_property(conceivable, X1) | ~ exemplifies_relation(greater_than, X1, X0) | ~ object(X1)) & exemplifies_property(conceivable, X0))) | ~ object(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (object(X0) => (exemplifies_property(none_greater, X0) <=> (~ ? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)) & exemplifies_property(conceivable, X0)))), inference(rectify, [], [f7])).
fof(f7, plain, ! [X0] : (object(X0) => (exemplifies_property(none_greater, X0) <=> (~ ? [X3] : (exemplifies_property(conceivable, X3) & exemplifies_relation(greater_than, X3, X0) & object(X3)) & exemplifies_property(conceivable, X0)))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI015+1.p', definition_none_greater)).
fof(f219, plain, (~ exemplifies_property(conceivable, god) | ~ exemplifies_property(none_greater, god)), inference(resolution, [], [f218, f117])).
fof(f117, plain, ! [X2, X0] : (~ exemplifies_relation(greater_than, X2, X0) | ~ exemplifies_property(conceivable, X2) | ~ exemplifies_property(none_greater, X0)), inference(subsumption_resolution, [], [f116, f63])).
fof(f116, plain, ! [X2, X0] : (~ exemplifies_property(conceivable, X2) | ~ exemplifies_relation(greater_than, X2, X0) | ~ exemplifies_property(none_greater, X0) | ~ object(X0)), inference(subsumption_resolution, [], [f90, f63])).
fof(f90, plain, ! [X2, X0] : (~ exemplifies_property(conceivable, X2) | ~ exemplifies_relation(greater_than, X2, X0) | ~ object(X2) | ~ exemplifies_property(none_greater, X0) | ~ object(X0)), inference(cnf_transformation, [], [f56])).
fof(f218, plain, exemplifies_relation(greater_than, god, god), inference(subsumption_resolution, [], [f217, f99])).
fof(f217, plain, (exemplifies_relation(greater_than, god, god) | ~ is_the(god, none_greater)), inference(subsumption_resolution, [], [f215, f100])).
fof(f100, plain, ~ exemplifies_property(existence, god), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ~ exemplifies_property(existence, god), inference(flattening, [], [f12])).
fof(f12, plain, ~ exemplifies_property(existence, god), inference(negated_conjecture, [], [f11])).
fof(f11, plain, ~ exemplifies_property(existence, god), file('/home/ubuntu/library/tptp/Problems/PHI/PHI015+1.p', god_exists)).
fof(f215, plain, (exemplifies_relation(greater_than, god, god) | exemplifies_property(existence, god) | ~ is_the(god, none_greater)), inference(superposition, [], [f120, f196])).
fof(f196, plain, (god = sK10(god)), inference(resolution, [], [f190, f148])).
fof(f148, plain, object(sK10(god)), inference(subsumption_resolution, [], [f147, f100])).
fof(f147, plain, (exemplifies_property(existence, god) | object(sK10(god))), inference(resolution, [], [f121, f99])).
fof(f121, plain, ! [X0] : (~ is_the(X0, none_greater) | exemplifies_property(existence, X0) | object(sK10(X0))), inference(subsumption_resolution, [], [f96, f65])).
fof(f96, plain, ! [X0] : (object(sK10(X0)) | exemplifies_property(existence, X0) | ~ is_the(X0, none_greater) | ~ object(X0)), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ! [X0] : ((exemplifies_property(conceivable, sK10(X0)) & exemplifies_relation(greater_than, sK10(X0), X0) & object(sK10(X0))) | exemplifies_property(existence, X0) | ~ is_the(X0, none_greater) | ~ object(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f30, f59])).
fof(f59, plain, ! [X0] : (? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)) => (exemplifies_property(conceivable, sK10(X0)) & exemplifies_relation(greater_than, sK10(X0), X0) & object(sK10(X0)))), introduced(choice_axiom, [])).
fof(f30, plain, ! [X0] : (? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)) | exemplifies_property(existence, X0) | ~ is_the(X0, none_greater) | ~ object(X0)), inference(flattening, [], [f29])).
fof(f29, plain, ! [X0] : ((? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)) | (exemplifies_property(existence, X0) | ~ is_the(X0, none_greater))) | ~ object(X0)), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (object(X0) => ((~ exemplifies_property(existence, X0) & is_the(X0, none_greater)) => ? [X1] : (exemplifies_property(conceivable, X1) & exemplifies_relation(greater_than, X1, X0) & object(X1)))), inference(rectify, [], [f9])).
fof(f9, plain, ! [X0] : (object(X0) => ((~ exemplifies_property(existence, X0) & is_the(X0, none_greater)) => ? [X3] : (exemplifies_property(conceivable, X3) & exemplifies_relation(greater_than, X3, X0) & object(X3)))), file('/home/ubuntu/library/tptp/Problems/PHI/PHI015+1.p', premise_2)).
fof(f190, plain, ! [X0] : (~ object(X0) | (god = X0)), inference(subsumption_resolution, [], [f189, f126])).
fof(f126, plain, object(god), inference(resolution, [], [f65, f99])).
fof(f189, plain, ! [X0] : (~ object(X0) | ~ object(god) | (god = X0)), inference(resolution, [], [f163, f169])).
fof(f163, plain, ! [X2, X0, X1] : (~ sP2(X0, X2) | ~ object(X1) | ~ object(X0) | (X0 = X1)), inference(subsumption_resolution, [], [f161, f138])).
fof(f138, plain, ! [X2, X3] : (~ sP2(X2, X3) | property(X3)), inference(resolution, [], [f81, f62])).
fof(f62, plain, ! [X0, X1] : (~ exemplifies_property(X1, X0) | property(X1)), inference(cnf_transformation, [], [f20])).
fof(f161, plain, ! [X2, X0, X1] : (~ object(X0) | ~ object(X1) | ~ property(X2) | ~ sP2(X0, X2) | (X0 = X1)), inference(resolution, [], [f87, f79])).
fof(f79, plain, ! [X2, X0, X1] : (~ sP3(X0, X1, X2) | ~ sP2(X1, X0) | (X1 = X2)), inference(cnf_transformation, [], [f46])).
fof(f120, plain, ! [X0] : (exemplifies_relation(greater_than, sK10(X0), X0) | exemplifies_property(existence, X0) | ~ is_the(X0, none_greater)), inference(subsumption_resolution, [], [f97, f65])).
fof(f97, plain, ! [X0] : (exemplifies_relation(greater_than, sK10(X0), X0) | exemplifies_property(existence, X0) | ~ is_the(X0, none_greater) | ~ object(X0)), inference(cnf_transformation, [], [f60])).