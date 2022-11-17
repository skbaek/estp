fof(f135942, plain, $false, inference(trivial_inequality_removal, [], [f135940])).
fof(f135940, plain, ~ (mult(sK6, mult(sK8, mult(sK7, sK8))) = mult(sK6, mult(sK8, mult(sK7, sK8)))), inference(backward_demodulation, [], [f123, f132635])).
fof(f132635, plain, ! [X263, X262] : (mult(mult(X262, X263), X262) = mult(X262, mult(X263, X262))), inference(forward_demodulation, [], [f123077, f87])).
fof(f87, plain, ! [X1] : (i(i(X1)) = X1), inference(superposition, [], [f53, f51])).
fof(f51, plain, ! [X1] : (i(X1) = ld(X1, unit)), inference(superposition, [], [f27, f34])).
fof(f34, plain, ! [X0] : (unit = mult(X0, i(X0))), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (unit = mult(X0, i(X0))), inference(rectify, [], [f9])).
fof(f9, plain, ! [X1] : (unit = mult(X1, i(X1))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', f09)).
fof(f27, plain, ! [X0, X1] : (ld(X1, mult(X1, X0)) = X0), inference(cnf_transformation, [], [f2])).
fof(f2, plain, ! [X0, X1] : (ld(X1, mult(X1, X0)) = X0), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', f02)).
fof(f53, plain, ! [X5] : (ld(i(X5), unit) = X5), inference(superposition, [], [f27, f35])).
fof(f35, plain, ! [X0] : (unit = mult(i(X0), X0)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ! [X0] : (unit = mult(i(X0), X0)), inference(rectify, [], [f10])).
fof(f10, plain, ! [A] : (mult(i(A), A) = unit), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', f10)).
fof(f123077, plain, ! [X263, X262] : (mult(mult(X262, X263), X262) = mult(i(i(X262)), mult(X263, X262))), inference(superposition, [], [f122591, f135])).
fof(f135, plain, ! [X19, X20] : (mult(i(X19), mult(mult(X19, X20), X19)) = mult(X20, X19)), inference(forward_demodulation, [], [f114, f31])).
fof(f31, plain, ! [X0] : (mult(unit, X0) = X0), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (mult(unit, X0) = X0), inference(rectify, [], [f6])).
fof(f6, plain, ! [X1] : (mult(unit, X1) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', f06)).
fof(f114, plain, ! [X19, X20] : (mult(i(X19), mult(mult(X19, X20), X19)) = mult(mult(unit, X20), X19)), inference(superposition, [], [f32, f35])).
fof(f32, plain, ! [X2, X0, X1] : (mult(mult(mult(X2, X1), X0), X1) = mult(X2, mult(mult(X1, X0), X1))), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1, X2] : (mult(mult(mult(X2, X1), X0), X1) = mult(X2, mult(mult(X1, X0), X1))), inference(rectify, [], [f7])).
fof(f7, plain, ! [X2, X0, X1] : (mult(mult(mult(X1, X0), X2), X0) = mult(X1, mult(mult(X0, X2), X0))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', f07)).
fof(f122591, plain, ! [X47, X48] : (mult(i(X47), mult(X47, X48)) = X48), inference(duplicate_literal_removal, [], [f122590])).
fof(f122590, plain, ! [X47, X48] : ((mult(i(X47), mult(X47, X48)) = X48) | (mult(i(X47), mult(X47, X48)) = X48)), inference(forward_demodulation, [], [f122589, f87])).
fof(f122589, plain, ! [X47, X48] : ((mult(i(X47), mult(X47, X48)) = X48) | (mult(i(X47), mult(i(i(X47)), X48)) = X48)), inference(forward_demodulation, [], [f122588, f27])).
fof(f122588, plain, ! [X47, X48] : ((mult(i(X47), mult(X47, X48)) = ld(X47, mult(X47, X48))) | (mult(i(X47), mult(i(i(X47)), X48)) = X48)), inference(forward_demodulation, [], [f122497, f87])).
fof(f122497, plain, ! [X47, X48] : ((ld(X47, mult(i(i(X47)), X48)) = mult(i(X47), mult(i(i(X47)), X48))) | (mult(i(X47), mult(i(i(X47)), X48)) = X48)), inference(duplicate_literal_removal, [], [f122292])).
fof(f122292, plain, ! [X47, X48] : ((ld(X47, mult(i(i(X47)), X48)) = mult(i(X47), mult(i(i(X47)), X48))) | (mult(i(X47), mult(i(i(X47)), X48)) = X48) | (ld(X47, mult(i(i(X47)), X48)) = mult(i(X47), mult(i(i(X47)), X48)))), inference(superposition, [], [f1357, f1116])).
fof(f1116, plain, ! [X8, X7] : ((ld(X7, X8) = mult(X8, i(X7))) | (ld(X7, X8) = mult(i(X7), X8))), inference(superposition, [], [f33, f160])).
fof(f160, plain, ! [X2, X3] : ((mult(ld(X2, X3), X2) = X3) | (ld(X2, X3) = mult(i(X2), X3))), inference(superposition, [], [f36, f26])).
fof(f26, plain, ! [X0, X1] : (mult(X1, ld(X1, X0)) = X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (mult(X1, ld(X1, X0)) = X0), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', f01)).
fof(f36, plain, ! [X0, X1] : ((mult(i(X1), mult(X1, X0)) = X0) | (mult(X1, X0) = mult(X0, X1))), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [B, A] : ((mult(A, B) = mult(B, A)) | (mult(i(A), mult(A, B)) = B)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', f11)).
fof(f33, plain, ! [X0, X1] : (mult(mult(X1, X0), i(X0)) = X1), inference(cnf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (mult(mult(X1, X0), i(X0)) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', f08)).
fof(f1357, plain, ! [X6, X5] : ((mult(X5, mult(i(X5), X6)) = mult(mult(i(X5), X6), X5)) | (mult(X5, mult(i(X5), X6)) = X6)), inference(superposition, [], [f170, f27])).
fof(f170, plain, ! [X8, X7] : ((mult(X7, X8) = ld(i(X7), X8)) | (mult(X7, X8) = mult(X8, X7))), inference(superposition, [], [f27, f36])).
fof(f123, plain, ~ (mult(sK6, mult(sK8, mult(sK7, sK8))) = mult(sK6, mult(mult(sK8, sK7), sK8))), inference(superposition, [], [f38, f32])).
fof(f38, plain, ~ (mult(sK6, mult(sK8, mult(sK7, sK8))) = mult(mult(mult(sK6, sK8), sK7), sK8)), inference(cnf_transformation, [], [f25])).
fof(f25, plain, (~ (mult(mult(sK2, sK0), mult(sK1, sK2)) = mult(sK2, mult(mult(sK0, sK1), sK2))) & ~ (mult(mult(sK5, sK3), mult(sK4, sK5)) = mult(mult(sK5, mult(sK3, sK4)), sK5)) & ~ (mult(sK6, mult(sK8, mult(sK7, sK8))) = mult(mult(mult(sK6, sK8), sK7), sK8)) & ~ (mult(sK11, mult(sK9, mult(sK11, sK10))) = mult(mult(mult(sK11, sK9), sK11), sK10))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK0, sK1, sK2, sK3, sK4, sK5, sK6, sK7, sK8, sK9, sK10, sK11])], [f20, f24, f23, f22, f21])).
fof(f21, plain, (? [X0, X1, X2] : ~ (mult(mult(X2, X0), mult(X1, X2)) = mult(X2, mult(mult(X0, X1), X2))) => ~ (mult(mult(sK2, sK0), mult(sK1, sK2)) = mult(sK2, mult(mult(sK0, sK1), sK2)))), introduced(choice_axiom, [])).
fof(f22, plain, (? [X3, X4, X5] : ~ (mult(mult(X5, X3), mult(X4, X5)) = mult(mult(X5, mult(X3, X4)), X5)) => ~ (mult(mult(sK5, sK3), mult(sK4, sK5)) = mult(mult(sK5, mult(sK3, sK4)), sK5))), introduced(choice_axiom, [])).
fof(f23, plain, (? [X6, X7, X8] : ~ (mult(X6, mult(X8, mult(X7, X8))) = mult(mult(mult(X6, X8), X7), X8)) => ~ (mult(sK6, mult(sK8, mult(sK7, sK8))) = mult(mult(mult(sK6, sK8), sK7), sK8))), introduced(choice_axiom, [])).
fof(f24, plain, (? [X9, X10, X11] : ~ (mult(X11, mult(X9, mult(X11, X10))) = mult(mult(mult(X11, X9), X11), X10)) => ~ (mult(sK11, mult(sK9, mult(sK11, sK10))) = mult(mult(mult(sK11, sK9), sK11), sK10))), introduced(choice_axiom, [])).
fof(f20, plain, (? [X0, X1, X2] : ~ (mult(mult(X2, X0), mult(X1, X2)) = mult(X2, mult(mult(X0, X1), X2))) & ? [X3, X4, X5] : ~ (mult(mult(X5, X3), mult(X4, X5)) = mult(mult(X5, mult(X3, X4)), X5)) & ? [X6, X7, X8] : ~ (mult(X6, mult(X8, mult(X7, X8))) = mult(mult(mult(X6, X8), X7), X8)) & ? [X9, X10, X11] : ~ (mult(X11, mult(X9, mult(X11, X10))) = mult(mult(mult(X11, X9), X11), X10))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ (! [X0, X1, X2] : (mult(mult(X2, X0), mult(X1, X2)) = mult(X2, mult(mult(X0, X1), X2))) | ! [X3, X4, X5] : (mult(mult(X5, X3), mult(X4, X5)) = mult(mult(X5, mult(X3, X4)), X5)) | ! [X6, X7, X8] : (mult(X6, mult(X8, mult(X7, X8))) = mult(mult(mult(X6, X8), X7), X8)) | ! [X9, X10, X11] : (mult(X11, mult(X9, mult(X11, X10))) = mult(mult(mult(X11, X9), X11), X10))), inference(rectify, [], [f13])).
fof(f13, plain, ~ (! [X12, X13, X14] : (mult(mult(X14, X12), mult(X13, X14)) = mult(X14, mult(mult(X12, X13), X14))) | ! [X9, X10, X11] : (mult(mult(X11, X9), mult(X10, X11)) = mult(mult(X11, mult(X9, X10)), X11)) | ! [X6, X7, X8] : (mult(X6, mult(X8, mult(X7, X8))) = mult(mult(mult(X6, X8), X7), X8)) | ! [X3, X4, X5] : (mult(X5, mult(X3, mult(X5, X4))) = mult(mult(mult(X5, X3), X5), X4))), inference(negated_conjecture, [], [f12])).
fof(f12, plain, ~ (! [X12, X13, X14] : (mult(mult(X14, X12), mult(X13, X14)) = mult(X14, mult(mult(X12, X13), X14))) | ! [X9, X10, X11] : (mult(mult(X11, X9), mult(X10, X11)) = mult(mult(X11, mult(X9, X10)), X11)) | ! [X6, X7, X8] : (mult(X6, mult(X8, mult(X7, X8))) = mult(mult(mult(X6, X8), X7), X8)) | ! [X3, X4, X5] : (mult(X5, mult(X3, mult(X5, X4))) = mult(mult(mult(X5, X3), X5), X4))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748+1.p', goals)).