fof(f2078, plain, $false, inference(avatar_sat_refutation, [], [f2062, f2063, f2064, f2065, f2066, f2067, f2068, f2069, f2070, f2071, f2072, f2073, f2074, f2075, f2076, f2077])).
fof(f2077, plain, (spl1_337 | spl1_282 | spl1_227 | spl1_172 | spl1_117 | spl1_62 | spl1_7), inference(avatar_split_clause, [], [f530, f564, f792, f1020, f1248, f1476, f1704, f1932])).
fof(f1932, plain, (spl1_337 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl1_337])])).
fof(f1704, plain, (spl1_282 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl1_282])])).
fof(f1476, plain, (spl1_227 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl1_227])])).
fof(f1248, plain, (spl1_172 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl1_172])])).
fof(f1020, plain, (spl1_117 <=> (e4 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl1_117])])).
fof(f792, plain, (spl1_62 <=> (e5 = op(e5, e5))), introduced(avatar_definition, [new_symbols(naming, [spl1_62])])).
fof(f564, plain, (spl1_7 <=> (e6 = op(e6, e6))), introduced(avatar_definition, [new_symbols(naming, [spl1_7])])).
fof(f530, plain, ((e6 = op(e6, e6)) | (e5 = op(e5, e5)) | (e4 = op(e4, e4)) | (e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ((((e6 = op(e6, e6)) & (e5 = op(e5, e5)) & (e4 = op(e4, e4)) & (e3 = op(e3, e3)) & (e2 = op(e2, e2)) & (e1 = op(e1, e1)) & (e0 = op(e0, e0))) | sP0) & (~ (e6 = op(e6, e6)) | ~ (e5 = op(e5, e5)) | ~ (e4 = op(e4, e4)) | ~ (e3 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (e0 = op(e0, e0))) & ((e6 = op(e6, e6)) | (e5 = op(e5, e5)) | (e4 = op(e4, e4)) | (e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(definition_folding, [], [f8, e9])).
fof(f9, plain, ((~ (e6 = op(e6, e6)) & ~ (e5 = op(e5, e5)) & ~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e9])).
fof(e9, plain, (sP0 <=> (~ (e6 = op(e6, e6)) & ~ (e5 = op(e5, e5)) & ~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f8, plain, ((((e6 = op(e6, e6)) & (e5 = op(e5, e5)) & (e4 = op(e4, e4)) & (e3 = op(e3, e3)) & (e2 = op(e2, e2)) & (e1 = op(e1, e1)) & (e0 = op(e0, e0))) | (~ (e6 = op(e6, e6)) & ~ (e5 = op(e5, e5)) & ~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e0)))) & (~ (e6 = op(e6, e6)) | ~ (e5 = op(e5, e5)) | ~ (e4 = op(e4, e4)) | ~ (e3 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (e0 = op(e0, e0))) & ((e6 = op(e6, e6)) | (e5 = op(e5, e5)) | (e4 = op(e4, e4)) | (e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ (((~ (e6 = op(e6, e6)) | ~ (e5 = op(e5, e5)) | ~ (e4 = op(e4, e4)) | ~ (e3 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (e0 = op(e0, e0))) & ((e6 = op(e6, e6)) | (e5 = op(e5, e5)) | (e4 = op(e4, e4)) | (e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0)))) | ((e6 = op(e6, e6)) & (e5 = op(e5, e5)) & (e4 = op(e4, e4)) & (e3 = op(e3, e3)) & (e2 = op(e2, e2)) & (e1 = op(e1, e1)) & (e0 = op(e0, e0))) | (~ (e6 = op(e6, e6)) & ~ (e5 = op(e5, e5)) & ~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f6])).
fof(f6, plain, ~ (((~ (e6 = op(e6, e6)) | ~ (e5 = op(e5, e5)) | ~ (e4 = op(e4, e4)) | ~ (e3 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (e0 = op(e0, e0))) & ((e6 = op(e6, e6)) | (e5 = op(e5, e5)) | (e4 = op(e4, e4)) | (e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0)))) | ((e6 = op(e6, e6)) & (e5 = op(e5, e5)) & (e4 = op(e4, e4)) & (e3 = op(e3, e3)) & (e2 = op(e2, e2)) & (e1 = op(e1, e1)) & (e0 = op(e0, e0))) | (~ (e6 = op(e6, e6)) & ~ (e5 = op(e5, e5)) & ~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG194+1.p', co1)).
fof(f2076, plain, (~ spl1_337 | ~ spl1_282 | ~ spl1_227 | ~ spl1_172 | ~ spl1_117 | ~ spl1_62 | ~ spl1_7), inference(avatar_split_clause, [], [f531, f564, f792, f1020, f1248, f1476, f1704, f1932])).
fof(f531, plain, (~ (e6 = op(e6, e6)) | ~ (e5 = op(e5, e5)) | ~ (e4 = op(e4, e4)) | ~ (e3 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (e0 = op(e0, e0))), inference(cnf_transformation, [], [f10])).
fof(f2075, plain, (spl1_344 | spl1_337), inference(avatar_split_clause, [], [f532, f1932, f2059])).
fof(f2059, plain, (spl1_344 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl1_344])])).
fof(f532, plain, ((e0 = op(e0, e0)) | sP0), inference(cnf_transformation, [], [f10])).
fof(f2074, plain, (spl1_344 | spl1_282), inference(avatar_split_clause, [], [f533, f1704, f2059])).
fof(f533, plain, ((e1 = op(e1, e1)) | sP0), inference(cnf_transformation, [], [f10])).
fof(f2073, plain, (spl1_344 | spl1_227), inference(avatar_split_clause, [], [f534, f1476, f2059])).
fof(f534, plain, ((e2 = op(e2, e2)) | sP0), inference(cnf_transformation, [], [f10])).
fof(f2072, plain, (spl1_344 | spl1_172), inference(avatar_split_clause, [], [f535, f1248, f2059])).
fof(f535, plain, ((e3 = op(e3, e3)) | sP0), inference(cnf_transformation, [], [f10])).
fof(f2071, plain, (spl1_344 | spl1_117), inference(avatar_split_clause, [], [f536, f1020, f2059])).
fof(f536, plain, ((e4 = op(e4, e4)) | sP0), inference(cnf_transformation, [], [f10])).
fof(f2070, plain, (spl1_344 | spl1_62), inference(avatar_split_clause, [], [f537, f792, f2059])).
fof(f537, plain, ((e5 = op(e5, e5)) | sP0), inference(cnf_transformation, [], [f10])).
fof(f2069, plain, (spl1_344 | spl1_7), inference(avatar_split_clause, [], [f538, f564, f2059])).
fof(f538, plain, ((e6 = op(e6, e6)) | sP0), inference(cnf_transformation, [], [f10])).
fof(f2068, plain, (~ spl1_344 | ~ spl1_337), inference(avatar_split_clause, [], [f523, f1932, f2059])).
fof(f523, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ((~ (e6 = op(e6, e6)) & ~ (e5 = op(e5, e5)) & ~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f9])).
fof(f2067, plain, (~ spl1_344 | ~ spl1_282), inference(avatar_split_clause, [], [f524, f1704, f2059])).
fof(f524, plain, (~ (e1 = op(e1, e1)) | ~ sP0), inference(cnf_transformation, [], [f11])).
fof(f2066, plain, (~ spl1_344 | ~ spl1_227), inference(avatar_split_clause, [], [f525, f1476, f2059])).
fof(f525, plain, (~ (e2 = op(e2, e2)) | ~ sP0), inference(cnf_transformation, [], [f11])).
fof(f2065, plain, (~ spl1_344 | ~ spl1_172), inference(avatar_split_clause, [], [f526, f1248, f2059])).
fof(f526, plain, (~ (e3 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f11])).
fof(f2064, plain, (~ spl1_344 | ~ spl1_117), inference(avatar_split_clause, [], [f527, f1020, f2059])).
fof(f527, plain, (~ (e4 = op(e4, e4)) | ~ sP0), inference(cnf_transformation, [], [f11])).
fof(f2063, plain, (~ spl1_344 | ~ spl1_62), inference(avatar_split_clause, [], [f528, f792, f2059])).
fof(f528, plain, (~ (e5 = op(e5, e5)) | ~ sP0), inference(cnf_transformation, [], [f11])).
fof(f2062, plain, (~ spl1_344 | ~ spl1_7), inference(avatar_split_clause, [], [f529, f564, f2059])).
fof(f529, plain, (~ (e6 = op(e6, e6)) | ~ sP0), inference(cnf_transformation, [], [f11])).