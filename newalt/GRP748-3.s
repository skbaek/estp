fof(f134425, plain, $false, inference(trivial_inequality_removal, [], [f134424])).
fof(f134424, plain, ~ (mult(a, mult(b, mult(c, b))) = mult(a, mult(b, mult(c, b)))), inference(backward_demodulation, [], [f79, f131122])).
fof(f131122, plain, ! [X263, X262] : (mult(mult(X262, X263), X262) = mult(X262, mult(X263, X262))), inference(forward_demodulation, [], [f121582, f59])).
fof(f59, plain, ! [X1] : (i(i(X1)) = X1), inference(superposition, [], [f25, f23])).
fof(f23, plain, ! [X1] : (i(X1) = ld(X1, unit)), inference(superposition, [], [f2, f9])).
fof(f9, plain, ! [X0] : (unit = mult(X0, i(X0))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', f09)).
fof(f2, plain, ! [X0, X1] : (ld(X0, mult(X0, X1)) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', f02)).
fof(f25, plain, ! [X5] : (ld(i(X5), unit) = X5), inference(superposition, [], [f2, f10])).
fof(f10, plain, ! [A] : (mult(i(A), A) = unit), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', f10)).
fof(f121582, plain, ! [X263, X262] : (mult(mult(X262, X263), X262) = mult(i(i(X262)), mult(X263, X262))), inference(superposition, [], [f121096, f91])).
fof(f91, plain, ! [X19, X20] : (mult(i(X19), mult(mult(X19, X20), X19)) = mult(X20, X19)), inference(forward_demodulation, [], [f70, f6])).
fof(f6, plain, ! [X0] : (mult(unit, X0) = X0), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', f06)).
fof(f70, plain, ! [X19, X20] : (mult(i(X19), mult(mult(X19, X20), X19)) = mult(mult(unit, X20), X19)), inference(superposition, [], [f7, f10])).
fof(f7, plain, ! [X2, X0, X1] : (mult(mult(mult(X0, X1), X2), X1) = mult(X0, mult(mult(X1, X2), X1))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', f07)).
fof(f121096, plain, ! [X47, X48] : (mult(i(X47), mult(X47, X48)) = X48), inference(duplicate_literal_removal, [], [f121095])).
fof(f121095, plain, ! [X47, X48] : ((mult(i(X47), mult(X47, X48)) = X48) | (mult(i(X47), mult(X47, X48)) = X48)), inference(forward_demodulation, [], [f121094, f59])).
fof(f121094, plain, ! [X47, X48] : ((mult(i(X47), mult(X47, X48)) = X48) | (mult(i(X47), mult(i(i(X47)), X48)) = X48)), inference(forward_demodulation, [], [f121093, f2])).
fof(f121093, plain, ! [X47, X48] : ((mult(i(X47), mult(X47, X48)) = ld(X47, mult(X47, X48))) | (mult(i(X47), mult(i(i(X47)), X48)) = X48)), inference(forward_demodulation, [], [f121002, f59])).
fof(f121002, plain, ! [X47, X48] : ((ld(X47, mult(i(i(X47)), X48)) = mult(i(X47), mult(i(i(X47)), X48))) | (mult(i(X47), mult(i(i(X47)), X48)) = X48)), inference(duplicate_literal_removal, [], [f120797])).
fof(f120797, plain, ! [X47, X48] : ((ld(X47, mult(i(i(X47)), X48)) = mult(i(X47), mult(i(i(X47)), X48))) | (mult(i(X47), mult(i(i(X47)), X48)) = X48) | (ld(X47, mult(i(i(X47)), X48)) = mult(i(X47), mult(i(i(X47)), X48)))), inference(superposition, [], [f1355, f1123])).
fof(f1123, plain, ! [X8, X7] : ((ld(X7, X8) = mult(X8, i(X7))) | (ld(X7, X8) = mult(i(X7), X8))), inference(superposition, [], [f8, f112])).
fof(f112, plain, ! [X2, X3] : ((mult(ld(X2, X3), X2) = X3) | (ld(X2, X3) = mult(i(X2), X3))), inference(superposition, [], [f11, f1])).
fof(f1, plain, ! [X0, X1] : (mult(X0, ld(X0, X1)) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', f01)).
fof(f11, plain, ! [A, B] : ((mult(A, B) = mult(B, A)) | (mult(i(A), mult(A, B)) = B)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', f11)).
fof(f8, plain, ! [X0, X1] : (mult(mult(X0, X1), i(X1)) = X0), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', f08)).
fof(f1355, plain, ! [X6, X5] : ((mult(X5, mult(i(X5), X6)) = mult(mult(i(X5), X6), X5)) | (mult(X5, mult(i(X5), X6)) = X6)), inference(superposition, [], [f122, f2])).
fof(f122, plain, ! [X8, X7] : ((mult(X7, X8) = ld(i(X7), X8)) | (mult(X7, X8) = mult(X8, X7))), inference(superposition, [], [f2, f11])).
fof(f79, plain, ~ (mult(a, mult(b, mult(c, b))) = mult(a, mult(mult(b, c), b))), inference(superposition, [], [f12, f7])).
fof(f12, plain, ~ (mult(a, mult(b, mult(c, b))) = mult(mult(mult(a, b), c), b)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP748-3.p', goals)).