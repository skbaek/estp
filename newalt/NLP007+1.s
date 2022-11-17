fof(f706, plain, $false, inference(avatar_sat_refutation, [], [f98, f103, f108, f113, f118, f123, f128, f133, f138, f143, f148, f153, f158, f163, f168, f173, f178, f183, f188, f193, f198, f203, f208, f213, f218, f223, f228, f233, f238, f243, f248, f257, f262, f267, f272, f277, f282, f287, f292, f297, f302, f307, f312, f317, f322, f327, f332, f337, f342, f347, f352, f357, f362, f367, f372, f377, f382, f387, f392, f397, f402, f407, f414, f417, f524, f532, f536, f543, f556, f603, f608, f691])).
fof(f691, plain, (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_6 | ~ spl22_7 | ~ spl22_8 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | spl22_12 | ~ spl22_13 | ~ spl22_14 | ~ spl22_15 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66), inference(avatar_contradiction_clause, [], [f690])).
fof(f690, plain, ($false | (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_6 | ~ spl22_7 | ~ spl22_8 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | spl22_12 | ~ spl22_13 | ~ spl22_14 | ~ spl22_15 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f689, f117])).
fof(f117, plain, (young(sK8) | ~ spl22_6), inference(avatar_component_clause, [], [f115])).
fof(f115, plain, (spl22_6 <=> young(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl22_6])])).
fof(f689, plain, (~ young(sK8) | (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_7 | ~ spl22_8 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | spl22_12 | ~ spl22_13 | ~ spl22_14 | ~ spl22_15 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f688, f122])).
fof(f122, plain, (man(sK8) | ~ spl22_7), inference(avatar_component_clause, [], [f120])).
fof(f120, plain, (spl22_7 <=> man(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl22_7])])).
fof(f688, plain, (~ man(sK8) | ~ young(sK8) | (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_8 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | spl22_12 | ~ spl22_13 | ~ spl22_14 | ~ spl22_15 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f687, f127])).
fof(f127, plain, (fellow(sK8) | ~ spl22_8), inference(avatar_component_clause, [], [f125])).
fof(f125, plain, (spl22_8 <=> fellow(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl22_8])])).
fof(f687, plain, (~ fellow(sK8) | ~ man(sK8) | ~ young(sK8) | (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | spl22_12 | ~ spl22_13 | ~ spl22_14 | ~ spl22_15 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f686, f147])).
fof(f147, plain, (~ (sK7 = sK8) | spl22_12), inference(avatar_component_clause, [], [f145])).
fof(f145, plain, (spl22_12 <=> (sK7 = sK8)), introduced(avatar_definition, [new_symbols(naming, [spl22_12])])).
fof(f686, plain, ((sK7 = sK8) | ~ fellow(sK8) | ~ man(sK8) | ~ young(sK8) | (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | ~ spl22_13 | ~ spl22_14 | ~ spl22_15 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f685, f162])).
fof(f162, plain, (seat(sK9) | ~ spl22_15), inference(avatar_component_clause, [], [f160])).
fof(f160, plain, (spl22_15 <=> seat(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl22_15])])).
fof(f685, plain, (~ seat(sK9) | (sK7 = sK8) | ~ fellow(sK8) | ~ man(sK8) | ~ young(sK8) | (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | ~ spl22_13 | ~ spl22_14 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f684, f152])).
fof(f152, plain, (front(sK9) | ~ spl22_13), inference(avatar_component_clause, [], [f150])).
fof(f150, plain, (spl22_13 <=> front(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl22_13])])).
fof(f684, plain, (~ front(sK9) | ~ seat(sK9) | (sK7 = sK8) | ~ fellow(sK8) | ~ man(sK8) | ~ young(sK8) | (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | ~ spl22_14 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f681, f157])).
fof(f157, plain, (furniture(sK9) | ~ spl22_14), inference(avatar_component_clause, [], [f155])).
fof(f155, plain, (spl22_14 <=> furniture(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl22_14])])).
fof(f681, plain, (~ furniture(sK9) | ~ front(sK9) | ~ seat(sK9) | (sK7 = sK8) | ~ fellow(sK8) | ~ man(sK8) | ~ young(sK8) | (~ spl22_2 | ~ spl22_3 | ~ spl22_4 | ~ spl22_5 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(resolution, [], [f647, f545])).
fof(f545, plain, (in(sK8, sK9) | (~ spl22_2 | ~ spl22_3)), inference(forward_demodulation, [], [f97, f102])).
fof(f102, plain, ((sK8 = sK11) | ~ spl22_3), inference(avatar_component_clause, [], [f100])).
fof(f100, plain, (spl22_3 <=> (sK8 = sK11)), introduced(avatar_definition, [new_symbols(naming, [spl22_3])])).
fof(f97, plain, (in(sK11, sK9) | ~ spl22_2), inference(avatar_component_clause, [], [f95])).
fof(f95, plain, (spl22_2 <=> in(sK11, sK9)), introduced(avatar_definition, [new_symbols(naming, [spl22_2])])).
fof(f647, plain, (! [X2, X3] : (~ in(X3, X2) | ~ furniture(X2) | ~ front(X2) | ~ seat(X2) | (sK7 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_4 | ~ spl22_5 | ~ spl22_9 | ~ spl22_10 | ~ spl22_11 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f646, f132])).
fof(f132, plain, (young(sK7) | ~ spl22_9), inference(avatar_component_clause, [], [f130])).
fof(f130, plain, (spl22_9 <=> young(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl22_9])])).
fof(f646, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ young(sK7) | ~ in(X3, X2) | (sK7 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_4 | ~ spl22_5 | ~ spl22_10 | ~ spl22_11 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f645, f137])).
fof(f137, plain, (man(sK7) | ~ spl22_10), inference(avatar_component_clause, [], [f135])).
fof(f135, plain, (spl22_10 <=> man(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl22_10])])).
fof(f645, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ man(sK7) | ~ young(sK7) | ~ in(X3, X2) | (sK7 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_4 | ~ spl22_5 | ~ spl22_11 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f644, f142])).
fof(f142, plain, (fellow(sK7) | ~ spl22_11), inference(avatar_component_clause, [], [f140])).
fof(f140, plain, (spl22_11 <=> fellow(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl22_11])])).
fof(f644, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ fellow(sK7) | ~ man(sK7) | ~ young(sK7) | ~ in(X3, X2) | (sK7 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_4 | ~ spl22_5 | ~ spl22_30 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f643, f237])).
fof(f237, plain, (front(sK2) | ~ spl22_30), inference(avatar_component_clause, [], [f235])).
fof(f235, plain, (spl22_30 <=> front(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl22_30])])).
fof(f643, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ front(sK2) | ~ fellow(sK7) | ~ man(sK7) | ~ young(sK7) | ~ in(X3, X2) | (sK7 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_4 | ~ spl22_5 | ~ spl22_31 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f642, f242])).
fof(f242, plain, (furniture(sK2) | ~ spl22_31), inference(avatar_component_clause, [], [f240])).
fof(f240, plain, (spl22_31 <=> furniture(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl22_31])])).
fof(f642, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ furniture(sK2) | ~ front(sK2) | ~ fellow(sK7) | ~ man(sK7) | ~ young(sK7) | ~ in(X3, X2) | (sK7 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_4 | ~ spl22_5 | ~ spl22_32 | ~ spl22_66)), inference(subsumption_resolution, [], [f610, f247])).
fof(f247, plain, (seat(sK2) | ~ spl22_32), inference(avatar_component_clause, [], [f245])).
fof(f245, plain, (spl22_32 <=> seat(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl22_32])])).
fof(f610, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ seat(sK2) | ~ furniture(sK2) | ~ front(sK2) | ~ fellow(sK7) | ~ man(sK7) | ~ young(sK7) | ~ in(X3, X2) | (sK7 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_4 | ~ spl22_5 | ~ spl22_66)), inference(resolution, [], [f413, f544])).
fof(f544, plain, (in(sK7, sK2) | (~ spl22_4 | ~ spl22_5)), inference(forward_demodulation, [], [f107, f112])).
fof(f112, plain, ((sK7 = sK10) | ~ spl22_5), inference(avatar_component_clause, [], [f110])).
fof(f110, plain, (spl22_5 <=> (sK7 = sK10)), introduced(avatar_definition, [new_symbols(naming, [spl22_5])])).
fof(f107, plain, (in(sK10, sK2) | ~ spl22_4), inference(avatar_component_clause, [], [f105])).
fof(f105, plain, (spl22_4 <=> in(sK10, sK2)), introduced(avatar_definition, [new_symbols(naming, [spl22_4])])).
fof(f413, plain, (! [X10, X19, X17, X18] : (~ in(X19, X17) | ~ seat(X10) | ~ furniture(X10) | ~ front(X10) | ~ seat(X17) | ~ furniture(X17) | ~ front(X17) | ~ fellow(X19) | ~ man(X19) | ~ young(X19) | ~ in(X18, X10) | (X18 = X19) | ~ fellow(X18) | ~ man(X18) | ~ young(X18)) | ~ spl22_66), inference(avatar_component_clause, [], [f412])).
fof(f412, plain, (spl22_66 <=> ! [X18, X17, X19, X10] : (~ in(X19, X17) | ~ seat(X10) | ~ furniture(X10) | ~ front(X10) | ~ seat(X17) | ~ furniture(X17) | ~ front(X17) | ~ fellow(X19) | ~ man(X19) | ~ young(X19) | ~ in(X18, X10) | (X18 = X19) | ~ fellow(X18) | ~ man(X18) | ~ young(X18))), introduced(avatar_definition, [new_symbols(naming, [spl22_66])])).
fof(f608, plain, (~ spl22_17 | ~ spl22_19 | ~ spl22_20 | ~ spl22_21 | ~ spl22_86), inference(avatar_contradiction_clause, [], [f607])).
fof(f607, plain, ($false | (~ spl22_17 | ~ spl22_19 | ~ spl22_20 | ~ spl22_21 | ~ spl22_86)), inference(subsumption_resolution, [], [f606, f187])).
fof(f187, plain, (way(sK6) | ~ spl22_20), inference(avatar_component_clause, [], [f185])).
fof(f185, plain, (spl22_20 <=> way(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl22_20])])).
fof(f606, plain, (~ way(sK6) | (~ spl22_17 | ~ spl22_19 | ~ spl22_21 | ~ spl22_86)), inference(subsumption_resolution, [], [f605, f182])).
fof(f182, plain, (lonely(sK6) | ~ spl22_19), inference(avatar_component_clause, [], [f180])).
fof(f180, plain, (spl22_19 <=> lonely(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl22_19])])).
fof(f605, plain, (~ lonely(sK6) | ~ way(sK6) | (~ spl22_17 | ~ spl22_21 | ~ spl22_86)), inference(subsumption_resolution, [], [f604, f192])).
fof(f192, plain, (street(sK6) | ~ spl22_21), inference(avatar_component_clause, [], [f190])).
fof(f190, plain, (spl22_21 <=> street(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl22_21])])).
fof(f604, plain, (~ street(sK6) | ~ lonely(sK6) | ~ way(sK6) | (~ spl22_17 | ~ spl22_86)), inference(resolution, [], [f552, f172])).
fof(f172, plain, (down(sK4, sK6) | ~ spl22_17), inference(avatar_component_clause, [], [f170])).
fof(f170, plain, (spl22_17 <=> down(sK4, sK6)), introduced(avatar_definition, [new_symbols(naming, [spl22_17])])).
fof(f552, plain, (! [X1] : (~ down(sK4, X1) | ~ street(X1) | ~ lonely(X1) | ~ way(X1)) | ~ spl22_86), inference(avatar_component_clause, [], [f551])).
fof(f551, plain, (spl22_86 <=> ! [X1] : (~ street(X1) | ~ down(sK4, X1) | ~ lonely(X1) | ~ way(X1))), introduced(avatar_definition, [new_symbols(naming, [spl22_86])])).
fof(f603, plain, (~ spl22_18 | ~ spl22_22 | ~ spl22_23 | ~ spl22_24 | ~ spl22_25 | ~ spl22_26 | ~ spl22_87), inference(avatar_contradiction_clause, [], [f602])).
fof(f602, plain, ($false | (~ spl22_18 | ~ spl22_22 | ~ spl22_23 | ~ spl22_24 | ~ spl22_25 | ~ spl22_26 | ~ spl22_87)), inference(subsumption_resolution, [], [f601, f212])).
fof(f212, plain, (car(sK5) | ~ spl22_25), inference(avatar_component_clause, [], [f210])).
fof(f210, plain, (spl22_25 <=> car(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl22_25])])).
fof(f601, plain, (~ car(sK5) | (~ spl22_18 | ~ spl22_22 | ~ spl22_23 | ~ spl22_24 | ~ spl22_26 | ~ spl22_87)), inference(subsumption_resolution, [], [f600, f207])).
fof(f207, plain, (white(sK5) | ~ spl22_24), inference(avatar_component_clause, [], [f205])).
fof(f205, plain, (spl22_24 <=> white(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl22_24])])).
fof(f600, plain, (~ white(sK5) | ~ car(sK5) | (~ spl22_18 | ~ spl22_22 | ~ spl22_23 | ~ spl22_26 | ~ spl22_87)), inference(subsumption_resolution, [], [f599, f202])).
fof(f202, plain, (dirty(sK5) | ~ spl22_23), inference(avatar_component_clause, [], [f200])).
fof(f200, plain, (spl22_23 <=> dirty(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl22_23])])).
fof(f599, plain, (~ dirty(sK5) | ~ white(sK5) | ~ car(sK5) | (~ spl22_18 | ~ spl22_22 | ~ spl22_26 | ~ spl22_87)), inference(subsumption_resolution, [], [f598, f197])).
fof(f197, plain, (old(sK5) | ~ spl22_22), inference(avatar_component_clause, [], [f195])).
fof(f195, plain, (spl22_22 <=> old(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl22_22])])).
fof(f598, plain, (~ old(sK5) | ~ dirty(sK5) | ~ white(sK5) | ~ car(sK5) | (~ spl22_18 | ~ spl22_26 | ~ spl22_87)), inference(subsumption_resolution, [], [f597, f217])).
fof(f217, plain, (chevy(sK5) | ~ spl22_26), inference(avatar_component_clause, [], [f215])).
fof(f215, plain, (spl22_26 <=> chevy(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl22_26])])).
fof(f597, plain, (~ chevy(sK5) | ~ old(sK5) | ~ dirty(sK5) | ~ white(sK5) | ~ car(sK5) | (~ spl22_18 | ~ spl22_87)), inference(resolution, [], [f555, f177])).
fof(f177, plain, (barrel(sK4, sK5) | ~ spl22_18), inference(avatar_component_clause, [], [f175])).
fof(f175, plain, (spl22_18 <=> barrel(sK4, sK5)), introduced(avatar_definition, [new_symbols(naming, [spl22_18])])).
fof(f555, plain, (! [X0] : (~ barrel(sK4, X0) | ~ chevy(X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0)) | ~ spl22_87), inference(avatar_component_clause, [], [f554])).
fof(f554, plain, (spl22_87 <=> ! [X0] : (~ chevy(X0) | ~ barrel(sK4, X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0))), introduced(avatar_definition, [new_symbols(naming, [spl22_87])])).
fof(f556, plain, (spl22_86 | spl22_87 | ~ spl22_16 | ~ spl22_27 | ~ spl22_28 | ~ spl22_29 | ~ spl22_65), inference(avatar_split_clause, [], [f549, f409, f230, f225, f220, f165, f554, f551])).
fof(f165, plain, (spl22_16 <=> in(sK4, sK3)), introduced(avatar_definition, [new_symbols(naming, [spl22_16])])).
fof(f220, plain, (spl22_27 <=> event(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl22_27])])).
fof(f225, plain, (spl22_28 <=> city(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl22_28])])).
fof(f230, plain, (spl22_29 <=> hollywood(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl22_29])])).
fof(f409, plain, (spl22_65 <=> ! [X11, X13, X12, X14] : (~ in(X12, X11) | ~ hollywood(X11) | ~ city(X11) | ~ event(X12) | ~ chevy(X13) | ~ car(X13) | ~ white(X13) | ~ dirty(X13) | ~ old(X13) | ~ street(X14) | ~ way(X14) | ~ lonely(X14) | ~ barrel(X12, X13) | ~ down(X12, X14))), introduced(avatar_definition, [new_symbols(naming, [spl22_65])])).
fof(f549, plain, (! [X0, X1] : (~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK4, X0) | ~ down(sK4, X1)) | (~ spl22_16 | ~ spl22_27 | ~ spl22_28 | ~ spl22_29 | ~ spl22_65)), inference(subsumption_resolution, [], [f548, f222])).
fof(f222, plain, (event(sK4) | ~ spl22_27), inference(avatar_component_clause, [], [f220])).
fof(f548, plain, (! [X0, X1] : (~ event(sK4) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK4, X0) | ~ down(sK4, X1)) | (~ spl22_16 | ~ spl22_28 | ~ spl22_29 | ~ spl22_65)), inference(subsumption_resolution, [], [f547, f227])).
fof(f227, plain, (city(sK3) | ~ spl22_28), inference(avatar_component_clause, [], [f225])).
fof(f547, plain, (! [X0, X1] : (~ city(sK3) | ~ event(sK4) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK4, X0) | ~ down(sK4, X1)) | (~ spl22_16 | ~ spl22_29 | ~ spl22_65)), inference(subsumption_resolution, [], [f546, f232])).
fof(f232, plain, (hollywood(sK3) | ~ spl22_29), inference(avatar_component_clause, [], [f230])).
fof(f546, plain, (! [X0, X1] : (~ hollywood(sK3) | ~ city(sK3) | ~ event(sK4) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK4, X0) | ~ down(sK4, X1)) | (~ spl22_16 | ~ spl22_65)), inference(resolution, [], [f167, f410])).
fof(f410, plain, (! [X14, X12, X13, X11] : (~ in(X12, X11) | ~ hollywood(X11) | ~ city(X11) | ~ event(X12) | ~ chevy(X13) | ~ car(X13) | ~ white(X13) | ~ dirty(X13) | ~ old(X13) | ~ street(X14) | ~ way(X14) | ~ lonely(X14) | ~ barrel(X12, X13) | ~ down(X12, X14)) | ~ spl22_65), inference(avatar_component_clause, [], [f409])).
fof(f167, plain, (in(sK4, sK3) | ~ spl22_16), inference(avatar_component_clause, [], [f165])).
fof(f543, plain, (~ spl22_50 | ~ spl22_54 | ~ spl22_55 | ~ spl22_56 | ~ spl22_57 | ~ spl22_58 | ~ spl22_68), inference(avatar_contradiction_clause, [], [f542])).
fof(f542, plain, ($false | (~ spl22_50 | ~ spl22_54 | ~ spl22_55 | ~ spl22_56 | ~ spl22_57 | ~ spl22_58 | ~ spl22_68)), inference(subsumption_resolution, [], [f541, f371])).
fof(f371, plain, (car(sK15) | ~ spl22_57), inference(avatar_component_clause, [], [f369])).
fof(f369, plain, (spl22_57 <=> car(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl22_57])])).
fof(f541, plain, (~ car(sK15) | (~ spl22_50 | ~ spl22_54 | ~ spl22_55 | ~ spl22_56 | ~ spl22_58 | ~ spl22_68)), inference(subsumption_resolution, [], [f540, f366])).
fof(f366, plain, (white(sK15) | ~ spl22_56), inference(avatar_component_clause, [], [f364])).
fof(f364, plain, (spl22_56 <=> white(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl22_56])])).
fof(f540, plain, (~ white(sK15) | ~ car(sK15) | (~ spl22_50 | ~ spl22_54 | ~ spl22_55 | ~ spl22_58 | ~ spl22_68)), inference(subsumption_resolution, [], [f539, f361])).
fof(f361, plain, (dirty(sK15) | ~ spl22_55), inference(avatar_component_clause, [], [f359])).
fof(f359, plain, (spl22_55 <=> dirty(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl22_55])])).
fof(f539, plain, (~ dirty(sK15) | ~ white(sK15) | ~ car(sK15) | (~ spl22_50 | ~ spl22_54 | ~ spl22_58 | ~ spl22_68)), inference(subsumption_resolution, [], [f538, f356])).
fof(f356, plain, (old(sK15) | ~ spl22_54), inference(avatar_component_clause, [], [f354])).
fof(f354, plain, (spl22_54 <=> old(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl22_54])])).
fof(f538, plain, (~ old(sK15) | ~ dirty(sK15) | ~ white(sK15) | ~ car(sK15) | (~ spl22_50 | ~ spl22_58 | ~ spl22_68)), inference(subsumption_resolution, [], [f537, f376])).
fof(f376, plain, (chevy(sK15) | ~ spl22_58), inference(avatar_component_clause, [], [f374])).
fof(f374, plain, (spl22_58 <=> chevy(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl22_58])])).
fof(f537, plain, (~ chevy(sK15) | ~ old(sK15) | ~ dirty(sK15) | ~ white(sK15) | ~ car(sK15) | (~ spl22_50 | ~ spl22_68)), inference(resolution, [], [f431, f336])).
fof(f336, plain, (barrel(sK14, sK15) | ~ spl22_50), inference(avatar_component_clause, [], [f334])).
fof(f334, plain, (spl22_50 <=> barrel(sK14, sK15)), introduced(avatar_definition, [new_symbols(naming, [spl22_50])])).
fof(f431, plain, (! [X0] : (~ barrel(sK14, X0) | ~ chevy(X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0)) | ~ spl22_68), inference(avatar_component_clause, [], [f430])).
fof(f430, plain, (spl22_68 <=> ! [X0] : (~ chevy(X0) | ~ barrel(sK14, X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0))), introduced(avatar_definition, [new_symbols(naming, [spl22_68])])).
fof(f536, plain, (spl22_67 | spl22_68 | ~ spl22_48 | ~ spl22_59 | ~ spl22_60 | ~ spl22_61 | ~ spl22_65), inference(avatar_split_clause, [], [f535, f409, f389, f384, f379, f324, f430, f427])).
fof(f427, plain, (spl22_67 <=> ! [X1] : (~ street(X1) | ~ down(sK14, X1) | ~ lonely(X1) | ~ way(X1))), introduced(avatar_definition, [new_symbols(naming, [spl22_67])])).
fof(f324, plain, (spl22_48 <=> in(sK14, sK13)), introduced(avatar_definition, [new_symbols(naming, [spl22_48])])).
fof(f379, plain, (spl22_59 <=> event(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl22_59])])).
fof(f384, plain, (spl22_60 <=> city(sK13)), introduced(avatar_definition, [new_symbols(naming, [spl22_60])])).
fof(f389, plain, (spl22_61 <=> hollywood(sK13)), introduced(avatar_definition, [new_symbols(naming, [spl22_61])])).
fof(f535, plain, (! [X0, X1] : (~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK14, X0) | ~ down(sK14, X1)) | (~ spl22_48 | ~ spl22_59 | ~ spl22_60 | ~ spl22_61 | ~ spl22_65)), inference(subsumption_resolution, [], [f534, f381])).
fof(f381, plain, (event(sK14) | ~ spl22_59), inference(avatar_component_clause, [], [f379])).
fof(f534, plain, (! [X0, X1] : (~ event(sK14) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK14, X0) | ~ down(sK14, X1)) | (~ spl22_48 | ~ spl22_60 | ~ spl22_61 | ~ spl22_65)), inference(subsumption_resolution, [], [f533, f386])).
fof(f386, plain, (city(sK13) | ~ spl22_60), inference(avatar_component_clause, [], [f384])).
fof(f533, plain, (! [X0, X1] : (~ city(sK13) | ~ event(sK14) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK14, X0) | ~ down(sK14, X1)) | (~ spl22_48 | ~ spl22_61 | ~ spl22_65)), inference(subsumption_resolution, [], [f525, f391])).
fof(f391, plain, (hollywood(sK13) | ~ spl22_61), inference(avatar_component_clause, [], [f389])).
fof(f525, plain, (! [X0, X1] : (~ hollywood(sK13) | ~ city(sK13) | ~ event(sK14) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK14, X0) | ~ down(sK14, X1)) | (~ spl22_48 | ~ spl22_65)), inference(resolution, [], [f410, f326])).
fof(f326, plain, (in(sK14, sK13) | ~ spl22_48), inference(avatar_component_clause, [], [f324])).
fof(f532, plain, (~ spl22_49 | ~ spl22_51 | ~ spl22_52 | ~ spl22_53 | ~ spl22_67), inference(avatar_contradiction_clause, [], [f531])).
fof(f531, plain, ($false | (~ spl22_49 | ~ spl22_51 | ~ spl22_52 | ~ spl22_53 | ~ spl22_67)), inference(subsumption_resolution, [], [f530, f346])).
fof(f346, plain, (way(sK16) | ~ spl22_52), inference(avatar_component_clause, [], [f344])).
fof(f344, plain, (spl22_52 <=> way(sK16)), introduced(avatar_definition, [new_symbols(naming, [spl22_52])])).
fof(f530, plain, (~ way(sK16) | (~ spl22_49 | ~ spl22_51 | ~ spl22_53 | ~ spl22_67)), inference(subsumption_resolution, [], [f529, f341])).
fof(f341, plain, (lonely(sK16) | ~ spl22_51), inference(avatar_component_clause, [], [f339])).
fof(f339, plain, (spl22_51 <=> lonely(sK16)), introduced(avatar_definition, [new_symbols(naming, [spl22_51])])).
fof(f529, plain, (~ lonely(sK16) | ~ way(sK16) | (~ spl22_49 | ~ spl22_53 | ~ spl22_67)), inference(subsumption_resolution, [], [f528, f351])).
fof(f351, plain, (street(sK16) | ~ spl22_53), inference(avatar_component_clause, [], [f349])).
fof(f349, plain, (spl22_53 <=> street(sK16)), introduced(avatar_definition, [new_symbols(naming, [spl22_53])])).
fof(f528, plain, (~ street(sK16) | ~ lonely(sK16) | ~ way(sK16) | (~ spl22_49 | ~ spl22_67)), inference(resolution, [], [f428, f331])).
fof(f331, plain, (down(sK14, sK16) | ~ spl22_49), inference(avatar_component_clause, [], [f329])).
fof(f329, plain, (spl22_49 <=> down(sK14, sK16)), introduced(avatar_definition, [new_symbols(naming, [spl22_49])])).
fof(f428, plain, (! [X1] : (~ down(sK14, X1) | ~ street(X1) | ~ lonely(X1) | ~ way(X1)) | ~ spl22_67), inference(avatar_component_clause, [], [f427])).
fof(f524, plain, (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_38 | ~ spl22_39 | ~ spl22_40 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | spl22_44 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_62 | ~ spl22_63 | ~ spl22_64 | ~ spl22_66), inference(avatar_contradiction_clause, [], [f523])).
fof(f523, plain, ($false | (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_38 | ~ spl22_39 | ~ spl22_40 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | spl22_44 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_62 | ~ spl22_63 | ~ spl22_64 | ~ spl22_66)), inference(subsumption_resolution, [], [f522, f276])).
fof(f276, plain, (young(sK18) | ~ spl22_38), inference(avatar_component_clause, [], [f274])).
fof(f274, plain, (spl22_38 <=> young(sK18)), introduced(avatar_definition, [new_symbols(naming, [spl22_38])])).
fof(f522, plain, (~ young(sK18) | (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_39 | ~ spl22_40 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | spl22_44 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_62 | ~ spl22_63 | ~ spl22_64 | ~ spl22_66)), inference(subsumption_resolution, [], [f521, f281])).
fof(f281, plain, (man(sK18) | ~ spl22_39), inference(avatar_component_clause, [], [f279])).
fof(f279, plain, (spl22_39 <=> man(sK18)), introduced(avatar_definition, [new_symbols(naming, [spl22_39])])).
fof(f521, plain, (~ man(sK18) | ~ young(sK18) | (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_40 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | spl22_44 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_62 | ~ spl22_63 | ~ spl22_64 | ~ spl22_66)), inference(subsumption_resolution, [], [f520, f286])).
fof(f286, plain, (fellow(sK18) | ~ spl22_40), inference(avatar_component_clause, [], [f284])).
fof(f284, plain, (spl22_40 <=> fellow(sK18)), introduced(avatar_definition, [new_symbols(naming, [spl22_40])])).
fof(f520, plain, (~ fellow(sK18) | ~ man(sK18) | ~ young(sK18) | (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | spl22_44 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_62 | ~ spl22_63 | ~ spl22_64 | ~ spl22_66)), inference(subsumption_resolution, [], [f519, f306])).
fof(f306, plain, (~ (sK17 = sK18) | spl22_44), inference(avatar_component_clause, [], [f304])).
fof(f304, plain, (spl22_44 <=> (sK17 = sK18)), introduced(avatar_definition, [new_symbols(naming, [spl22_44])])).
fof(f519, plain, ((sK17 = sK18) | ~ fellow(sK18) | ~ man(sK18) | ~ young(sK18) | (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_62 | ~ spl22_63 | ~ spl22_64 | ~ spl22_66)), inference(subsumption_resolution, [], [f518, f406])).
fof(f406, plain, (seat(sK12) | ~ spl22_64), inference(avatar_component_clause, [], [f404])).
fof(f404, plain, (spl22_64 <=> seat(sK12)), introduced(avatar_definition, [new_symbols(naming, [spl22_64])])).
fof(f518, plain, (~ seat(sK12) | (sK17 = sK18) | ~ fellow(sK18) | ~ man(sK18) | ~ young(sK18) | (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_62 | ~ spl22_63 | ~ spl22_66)), inference(subsumption_resolution, [], [f517, f396])).
fof(f396, plain, (front(sK12) | ~ spl22_62), inference(avatar_component_clause, [], [f394])).
fof(f394, plain, (spl22_62 <=> front(sK12)), introduced(avatar_definition, [new_symbols(naming, [spl22_62])])).
fof(f517, plain, (~ front(sK12) | ~ seat(sK12) | (sK17 = sK18) | ~ fellow(sK18) | ~ man(sK18) | ~ young(sK18) | (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_63 | ~ spl22_66)), inference(subsumption_resolution, [], [f516, f401])).
fof(f401, plain, (furniture(sK12) | ~ spl22_63), inference(avatar_component_clause, [], [f399])).
fof(f399, plain, (spl22_63 <=> furniture(sK12)), introduced(avatar_definition, [new_symbols(naming, [spl22_63])])).
fof(f516, plain, (~ furniture(sK12) | ~ front(sK12) | ~ seat(sK12) | (sK17 = sK18) | ~ fellow(sK18) | ~ man(sK18) | ~ young(sK18) | (~ spl22_34 | ~ spl22_35 | ~ spl22_36 | ~ spl22_37 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_66)), inference(resolution, [], [f507, f419])).
fof(f419, plain, (in(sK18, sK12) | (~ spl22_34 | ~ spl22_35)), inference(forward_demodulation, [], [f256, f261])).
fof(f261, plain, ((sK18 = sK21) | ~ spl22_35), inference(avatar_component_clause, [], [f259])).
fof(f259, plain, (spl22_35 <=> (sK18 = sK21)), introduced(avatar_definition, [new_symbols(naming, [spl22_35])])).
fof(f256, plain, (in(sK21, sK12) | ~ spl22_34), inference(avatar_component_clause, [], [f254])).
fof(f254, plain, (spl22_34 <=> in(sK21, sK12)), introduced(avatar_definition, [new_symbols(naming, [spl22_34])])).
fof(f507, plain, (! [X2, X3] : (~ in(X3, X2) | ~ furniture(X2) | ~ front(X2) | ~ seat(X2) | (sK17 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_36 | ~ spl22_37 | ~ spl22_41 | ~ spl22_42 | ~ spl22_43 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_66)), inference(subsumption_resolution, [], [f506, f291])).
fof(f291, plain, (young(sK17) | ~ spl22_41), inference(avatar_component_clause, [], [f289])).
fof(f289, plain, (spl22_41 <=> young(sK17)), introduced(avatar_definition, [new_symbols(naming, [spl22_41])])).
fof(f506, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ young(sK17) | ~ in(X3, X2) | (sK17 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_36 | ~ spl22_37 | ~ spl22_42 | ~ spl22_43 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_66)), inference(subsumption_resolution, [], [f505, f296])).
fof(f296, plain, (man(sK17) | ~ spl22_42), inference(avatar_component_clause, [], [f294])).
fof(f294, plain, (spl22_42 <=> man(sK17)), introduced(avatar_definition, [new_symbols(naming, [spl22_42])])).
fof(f505, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ man(sK17) | ~ young(sK17) | ~ in(X3, X2) | (sK17 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_36 | ~ spl22_37 | ~ spl22_43 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_66)), inference(subsumption_resolution, [], [f504, f301])).
fof(f301, plain, (fellow(sK17) | ~ spl22_43), inference(avatar_component_clause, [], [f299])).
fof(f299, plain, (spl22_43 <=> fellow(sK17)), introduced(avatar_definition, [new_symbols(naming, [spl22_43])])).
fof(f504, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ fellow(sK17) | ~ man(sK17) | ~ young(sK17) | ~ in(X3, X2) | (sK17 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_36 | ~ spl22_37 | ~ spl22_45 | ~ spl22_46 | ~ spl22_47 | ~ spl22_66)), inference(subsumption_resolution, [], [f503, f311])).
fof(f311, plain, (front(sK19) | ~ spl22_45), inference(avatar_component_clause, [], [f309])).
fof(f309, plain, (spl22_45 <=> front(sK19)), introduced(avatar_definition, [new_symbols(naming, [spl22_45])])).
fof(f503, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ front(sK19) | ~ fellow(sK17) | ~ man(sK17) | ~ young(sK17) | ~ in(X3, X2) | (sK17 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_36 | ~ spl22_37 | ~ spl22_46 | ~ spl22_47 | ~ spl22_66)), inference(subsumption_resolution, [], [f502, f316])).
fof(f316, plain, (furniture(sK19) | ~ spl22_46), inference(avatar_component_clause, [], [f314])).
fof(f314, plain, (spl22_46 <=> furniture(sK19)), introduced(avatar_definition, [new_symbols(naming, [spl22_46])])).
fof(f502, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ furniture(sK19) | ~ front(sK19) | ~ fellow(sK17) | ~ man(sK17) | ~ young(sK17) | ~ in(X3, X2) | (sK17 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_36 | ~ spl22_37 | ~ spl22_47 | ~ spl22_66)), inference(subsumption_resolution, [], [f472, f321])).
fof(f321, plain, (seat(sK19) | ~ spl22_47), inference(avatar_component_clause, [], [f319])).
fof(f319, plain, (spl22_47 <=> seat(sK19)), introduced(avatar_definition, [new_symbols(naming, [spl22_47])])).
fof(f472, plain, (! [X2, X3] : (~ seat(X2) | ~ furniture(X2) | ~ front(X2) | ~ seat(sK19) | ~ furniture(sK19) | ~ front(sK19) | ~ fellow(sK17) | ~ man(sK17) | ~ young(sK17) | ~ in(X3, X2) | (sK17 = X3) | ~ fellow(X3) | ~ man(X3) | ~ young(X3)) | (~ spl22_36 | ~ spl22_37 | ~ spl22_66)), inference(resolution, [], [f413, f418])).
fof(f418, plain, (in(sK17, sK19) | (~ spl22_36 | ~ spl22_37)), inference(forward_demodulation, [], [f266, f271])).
fof(f271, plain, ((sK17 = sK20) | ~ spl22_37), inference(avatar_component_clause, [], [f269])).
fof(f269, plain, (spl22_37 <=> (sK17 = sK20)), introduced(avatar_definition, [new_symbols(naming, [spl22_37])])).
fof(f266, plain, (in(sK20, sK19) | ~ spl22_36), inference(avatar_component_clause, [], [f264])).
fof(f264, plain, (spl22_36 <=> in(sK20, sK19)), introduced(avatar_definition, [new_symbols(naming, [spl22_36])])).
fof(f417, plain, (spl22_33 | spl22_1), inference(avatar_split_clause, [], [f78, f91, f250])).
fof(f250, plain, (spl22_33 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl22_33])])).
fof(f91, plain, (spl22_1 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl22_1])])).
fof(f78, plain, (sP1 | sP0), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((! [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (~ in(X9, X0) | ~ (X6 = X9) | ~ in(X8, X7) | ~ (X5 = X8) | ~ young(X6) | ~ man(X6) | ~ fellow(X6) | ~ young(X5) | ~ man(X5) | ~ fellow(X5) | (X5 = X6) | ~ front(X7) | ~ furniture(X7) | ~ seat(X7) | ~ in(X2, X1) | ~ down(X2, X4) | ~ barrel(X2, X3) | ~ lonely(X4) | ~ way(X4) | ~ street(X4) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ event(X2) | ~ city(X1) | ~ hollywood(X1) | ~ front(X0) | ~ furniture(X0) | ~ seat(X0)) & sP1) | (! [X10, X11, X12, X13, X14, X15, X16, X17, X18, X19] : (~ in(X19, X17) | ~ (X16 = X19) | ~ in(X18, X10) | ~ (X15 = X18) | ~ young(X16) | ~ man(X16) | ~ fellow(X16) | ~ young(X15) | ~ man(X15) | ~ fellow(X15) | (X15 = X16) | ~ front(X17) | ~ furniture(X17) | ~ seat(X17) | ~ in(X12, X11) | ~ down(X12, X14) | ~ barrel(X12, X13) | ~ lonely(X14) | ~ way(X14) | ~ street(X14) | ~ old(X13) | ~ dirty(X13) | ~ white(X13) | ~ car(X13) | ~ chevy(X13) | ~ event(X12) | ~ city(X11) | ~ hollywood(X11) | ~ front(X10) | ~ furniture(X10) | ~ seat(X10)) & sP0)), inference(rectify, [], [f7])).
fof(f7, plain, ((! [X10, X11, X12, X13, X14, X15, X16, X17, X18, X19] : (~ in(X19, X10) | ~ (X16 = X19) | ~ in(X18, X17) | ~ (X15 = X18) | ~ young(X16) | ~ man(X16) | ~ fellow(X16) | ~ young(X15) | ~ man(X15) | ~ fellow(X15) | (X15 = X16) | ~ front(X17) | ~ furniture(X17) | ~ seat(X17) | ~ in(X12, X11) | ~ down(X12, X14) | ~ barrel(X12, X13) | ~ lonely(X14) | ~ way(X14) | ~ street(X14) | ~ old(X13) | ~ dirty(X13) | ~ white(X13) | ~ car(X13) | ~ chevy(X13) | ~ event(X12) | ~ city(X11) | ~ hollywood(X11) | ~ front(X10) | ~ furniture(X10) | ~ seat(X10)) & sP1) | (! [X30, X31, X32, X33, X34, X35, X36, X37, X38, X39] : (~ in(X39, X37) | ~ (X36 = X39) | ~ in(X38, X30) | ~ (X35 = X38) | ~ young(X36) | ~ man(X36) | ~ fellow(X36) | ~ young(X35) | ~ man(X35) | ~ fellow(X35) | (X35 = X36) | ~ front(X37) | ~ furniture(X37) | ~ seat(X37) | ~ in(X32, X31) | ~ down(X32, X34) | ~ barrel(X32, X33) | ~ lonely(X34) | ~ way(X34) | ~ street(X34) | ~ old(X33) | ~ dirty(X33) | ~ white(X33) | ~ car(X33) | ~ chevy(X33) | ~ event(X32) | ~ city(X31) | ~ hollywood(X31) | ~ front(X30) | ~ furniture(X30) | ~ seat(X30)) & sP0)), inference(definition_folding, [], [f4, e6, e5])).
fof(f5, plain, (? [X20, X21, X22, X23, X24, X25, X26, X27, X28, X29] : (in(X29, X20) & (X26 = X29) & in(X28, X27) & (X25 = X28) & young(X26) & man(X26) & fellow(X26) & young(X25) & man(X25) & fellow(X25) & ~ (X25 = X26) & front(X27) & furniture(X27) & seat(X27) & in(X22, X21) & down(X22, X24) & barrel(X22, X23) & lonely(X24) & way(X24) & street(X24) & old(X23) & dirty(X23) & white(X23) & car(X23) & chevy(X23) & event(X22) & city(X21) & hollywood(X21) & front(X20) & furniture(X20) & seat(X20)) | ~ sP0), inference(usedef, [], [e5])).
fof(e5, plain, (sP0 <=> ? [X20, X21, X22, X23, X24, X25, X26, X27, X28, X29] : (in(X29, X20) & (X26 = X29) & in(X28, X27) & (X25 = X28) & young(X26) & man(X26) & fellow(X26) & young(X25) & man(X25) & fellow(X25) & ~ (X25 = X26) & front(X27) & furniture(X27) & seat(X27) & in(X22, X21) & down(X22, X24) & barrel(X22, X23) & lonely(X24) & way(X24) & street(X24) & old(X23) & dirty(X23) & white(X23) & car(X23) & chevy(X23) & event(X22) & city(X21) & hollywood(X21) & front(X20) & furniture(X20) & seat(X20))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f6, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X7) & (X6 = X9) & in(X8, X0) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0)) | ~ sP1), inference(usedef, [], [e6])).
fof(e6, plain, (sP1 <=> ? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X7) & (X6 = X9) & in(X8, X0) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f4, plain, ((! [X10, X11, X12, X13, X14, X15, X16, X17, X18, X19] : (~ in(X19, X10) | ~ (X16 = X19) | ~ in(X18, X17) | ~ (X15 = X18) | ~ young(X16) | ~ man(X16) | ~ fellow(X16) | ~ young(X15) | ~ man(X15) | ~ fellow(X15) | (X15 = X16) | ~ front(X17) | ~ furniture(X17) | ~ seat(X17) | ~ in(X12, X11) | ~ down(X12, X14) | ~ barrel(X12, X13) | ~ lonely(X14) | ~ way(X14) | ~ street(X14) | ~ old(X13) | ~ dirty(X13) | ~ white(X13) | ~ car(X13) | ~ chevy(X13) | ~ event(X12) | ~ city(X11) | ~ hollywood(X11) | ~ front(X10) | ~ furniture(X10) | ~ seat(X10)) & ? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X7) & (X6 = X9) & in(X8, X0) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0))) | (! [X30, X31, X32, X33, X34, X35, X36, X37, X38, X39] : (~ in(X39, X37) | ~ (X36 = X39) | ~ in(X38, X30) | ~ (X35 = X38) | ~ young(X36) | ~ man(X36) | ~ fellow(X36) | ~ young(X35) | ~ man(X35) | ~ fellow(X35) | (X35 = X36) | ~ front(X37) | ~ furniture(X37) | ~ seat(X37) | ~ in(X32, X31) | ~ down(X32, X34) | ~ barrel(X32, X33) | ~ lonely(X34) | ~ way(X34) | ~ street(X34) | ~ old(X33) | ~ dirty(X33) | ~ white(X33) | ~ car(X33) | ~ chevy(X33) | ~ event(X32) | ~ city(X31) | ~ hollywood(X31) | ~ front(X30) | ~ furniture(X30) | ~ seat(X30)) & ? [X20, X21, X22, X23, X24, X25, X26, X27, X28, X29] : (in(X29, X20) & (X26 = X29) & in(X28, X27) & (X25 = X28) & young(X26) & man(X26) & fellow(X26) & young(X25) & man(X25) & fellow(X25) & ~ (X25 = X26) & front(X27) & furniture(X27) & seat(X27) & in(X22, X21) & down(X22, X24) & barrel(X22, X23) & lonely(X24) & way(X24) & street(X24) & old(X23) & dirty(X23) & white(X23) & car(X23) & chevy(X23) & event(X22) & city(X21) & hollywood(X21) & front(X20) & furniture(X20) & seat(X20)))), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ~ ((? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X7) & (X6 = X9) & in(X8, X0) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0)) => ? [X10, X11, X12, X13, X14, X15, X16, X17, X18, X19] : (in(X19, X10) & (X16 = X19) & in(X18, X17) & (X15 = X18) & young(X16) & man(X16) & fellow(X16) & young(X15) & man(X15) & fellow(X15) & ~ (X15 = X16) & front(X17) & furniture(X17) & seat(X17) & in(X12, X11) & down(X12, X14) & barrel(X12, X13) & lonely(X14) & way(X14) & street(X14) & old(X13) & dirty(X13) & white(X13) & car(X13) & chevy(X13) & event(X12) & city(X11) & hollywood(X11) & front(X10) & furniture(X10) & seat(X10))) & (? [X20, X21, X22, X23, X24, X25, X26, X27, X28, X29] : (in(X29, X20) & (X26 = X29) & in(X28, X27) & (X25 = X28) & young(X26) & man(X26) & fellow(X26) & young(X25) & man(X25) & fellow(X25) & ~ (X25 = X26) & front(X27) & furniture(X27) & seat(X27) & in(X22, X21) & down(X22, X24) & barrel(X22, X23) & lonely(X24) & way(X24) & street(X24) & old(X23) & dirty(X23) & white(X23) & car(X23) & chevy(X23) & event(X22) & city(X21) & hollywood(X21) & front(X20) & furniture(X20) & seat(X20)) => ? [X30, X31, X32, X33, X34, X35, X36, X37, X38, X39] : (in(X39, X37) & (X36 = X39) & in(X38, X30) & (X35 = X38) & young(X36) & man(X36) & fellow(X36) & young(X35) & man(X35) & fellow(X35) & ~ (X35 = X36) & front(X37) & furniture(X37) & seat(X37) & in(X32, X31) & down(X32, X34) & barrel(X32, X33) & lonely(X34) & way(X34) & street(X34) & old(X33) & dirty(X33) & white(X33) & car(X33) & chevy(X33) & event(X32) & city(X31) & hollywood(X31) & front(X30) & furniture(X30) & seat(X30)))), inference(rectify, [], [f2])).
fof(f2, plain, ~ ((? [X20, X21, X22, X23, X24, X25, X26, X27, X28, X29] : (in(X29, X27) & (X26 = X29) & in(X28, X20) & (X25 = X28) & young(X26) & man(X26) & fellow(X26) & young(X25) & man(X25) & fellow(X25) & ~ (X25 = X26) & front(X27) & furniture(X27) & seat(X27) & in(X22, X21) & down(X22, X24) & barrel(X22, X23) & lonely(X24) & way(X24) & street(X24) & old(X23) & dirty(X23) & white(X23) & car(X23) & chevy(X23) & event(X22) & city(X21) & hollywood(X21) & front(X20) & furniture(X20) & seat(X20)) => ? [X30, X31, X32, X33, X34, X35, X36, X37, X38, X39] : (in(X39, X30) & (X36 = X39) & in(X38, X37) & (X35 = X38) & young(X36) & man(X36) & fellow(X36) & young(X35) & man(X35) & fellow(X35) & ~ (X35 = X36) & front(X37) & furniture(X37) & seat(X37) & in(X32, X31) & down(X32, X34) & barrel(X32, X33) & lonely(X34) & way(X34) & street(X34) & old(X33) & dirty(X33) & white(X33) & car(X33) & chevy(X33) & event(X32) & city(X31) & hollywood(X31) & front(X30) & furniture(X30) & seat(X30))) & (? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X0) & (X6 = X9) & in(X8, X7) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0)) => ? [X10, X11, X12, X13, X14, X15, X16, X17, X18, X19] : (in(X19, X17) & (X16 = X19) & in(X18, X10) & (X15 = X18) & young(X16) & man(X16) & fellow(X16) & young(X15) & man(X15) & fellow(X15) & ~ (X15 = X16) & front(X17) & furniture(X17) & seat(X17) & in(X12, X11) & down(X12, X14) & barrel(X12, X13) & lonely(X14) & way(X14) & street(X14) & old(X13) & dirty(X13) & white(X13) & car(X13) & chevy(X13) & event(X12) & city(X11) & hollywood(X11) & front(X10) & furniture(X10) & seat(X10)))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ((? [X20, X21, X22, X23, X24, X25, X26, X27, X28, X29] : (in(X29, X27) & (X26 = X29) & in(X28, X20) & (X25 = X28) & young(X26) & man(X26) & fellow(X26) & young(X25) & man(X25) & fellow(X25) & ~ (X25 = X26) & front(X27) & furniture(X27) & seat(X27) & in(X22, X21) & down(X22, X24) & barrel(X22, X23) & lonely(X24) & way(X24) & street(X24) & old(X23) & dirty(X23) & white(X23) & car(X23) & chevy(X23) & event(X22) & city(X21) & hollywood(X21) & front(X20) & furniture(X20) & seat(X20)) => ? [X30, X31, X32, X33, X34, X35, X36, X37, X38, X39] : (in(X39, X30) & (X36 = X39) & in(X38, X37) & (X35 = X38) & young(X36) & man(X36) & fellow(X36) & young(X35) & man(X35) & fellow(X35) & ~ (X35 = X36) & front(X37) & furniture(X37) & seat(X37) & in(X32, X31) & down(X32, X34) & barrel(X32, X33) & lonely(X34) & way(X34) & street(X34) & old(X33) & dirty(X33) & white(X33) & car(X33) & chevy(X33) & event(X32) & city(X31) & hollywood(X31) & front(X30) & furniture(X30) & seat(X30))) & (? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X0) & (X6 = X9) & in(X8, X7) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0)) => ? [X10, X11, X12, X13, X14, X15, X16, X17, X18, X19] : (in(X19, X17) & (X16 = X19) & in(X18, X10) & (X15 = X18) & young(X16) & man(X16) & fellow(X16) & young(X15) & man(X15) & fellow(X15) & ~ (X15 = X16) & front(X17) & furniture(X17) & seat(X17) & in(X12, X11) & down(X12, X14) & barrel(X12, X13) & lonely(X14) & way(X14) & street(X14) & old(X13) & dirty(X13) & white(X13) & car(X13) & chevy(X13) & event(X12) & city(X11) & hollywood(X11) & front(X10) & furniture(X10) & seat(X10)))), file('/home/ubuntu/library/tptp/Problems/NLP/NLP007+1.p', co1)).
fof(f414, plain, (spl22_65 | spl22_66 | spl22_65 | spl22_66), inference(avatar_split_clause, [], [f85, f412, f409, f412, f409])).
fof(f85, plain, ! [X4, X2, X0, X14, X12, X10, X8, X19, X17, X7, X3, X1, X13, X11, X9, X18] : (~ in(X9, X0) | ~ in(X8, X7) | ~ young(X9) | ~ man(X9) | ~ fellow(X9) | ~ young(X8) | ~ man(X8) | ~ fellow(X8) | (X8 = X9) | ~ front(X7) | ~ furniture(X7) | ~ seat(X7) | ~ in(X2, X1) | ~ down(X2, X4) | ~ barrel(X2, X3) | ~ lonely(X4) | ~ way(X4) | ~ street(X4) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ event(X2) | ~ city(X1) | ~ hollywood(X1) | ~ front(X0) | ~ furniture(X0) | ~ seat(X0) | ~ in(X19, X17) | ~ in(X18, X10) | ~ young(X19) | ~ man(X19) | ~ fellow(X19) | ~ young(X18) | ~ man(X18) | ~ fellow(X18) | (X18 = X19) | ~ front(X17) | ~ furniture(X17) | ~ seat(X17) | ~ in(X12, X11) | ~ down(X12, X14) | ~ barrel(X12, X13) | ~ lonely(X14) | ~ way(X14) | ~ street(X14) | ~ old(X13) | ~ dirty(X13) | ~ white(X13) | ~ car(X13) | ~ chevy(X13) | ~ event(X12) | ~ city(X11) | ~ hollywood(X11) | ~ front(X10) | ~ furniture(X10) | ~ seat(X10)), inference(equality_resolution, [], [f84])).
fof(f84, plain, ! [X4, X2, X0, X14, X12, X10, X8, X19, X17, X7, X3, X1, X15, X13, X11, X9, X18] : (~ in(X9, X0) | ~ in(X8, X7) | ~ young(X9) | ~ man(X9) | ~ fellow(X9) | ~ young(X8) | ~ man(X8) | ~ fellow(X8) | (X8 = X9) | ~ front(X7) | ~ furniture(X7) | ~ seat(X7) | ~ in(X2, X1) | ~ down(X2, X4) | ~ barrel(X2, X3) | ~ lonely(X4) | ~ way(X4) | ~ street(X4) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ event(X2) | ~ city(X1) | ~ hollywood(X1) | ~ front(X0) | ~ furniture(X0) | ~ seat(X0) | ~ in(X19, X17) | ~ in(X18, X10) | ~ (X15 = X18) | ~ young(X19) | ~ man(X19) | ~ fellow(X19) | ~ young(X15) | ~ man(X15) | ~ fellow(X15) | (X15 = X19) | ~ front(X17) | ~ furniture(X17) | ~ seat(X17) | ~ in(X12, X11) | ~ down(X12, X14) | ~ barrel(X12, X13) | ~ lonely(X14) | ~ way(X14) | ~ street(X14) | ~ old(X13) | ~ dirty(X13) | ~ white(X13) | ~ car(X13) | ~ chevy(X13) | ~ event(X12) | ~ city(X11) | ~ hollywood(X11) | ~ front(X10) | ~ furniture(X10) | ~ seat(X10)), inference(equality_resolution, [], [f83])).
fof(f83, plain, ! [X4, X2, X0, X14, X12, X10, X8, X19, X17, X7, X3, X1, X15, X13, X11, X9, X18, X16] : (~ in(X9, X0) | ~ in(X8, X7) | ~ young(X9) | ~ man(X9) | ~ fellow(X9) | ~ young(X8) | ~ man(X8) | ~ fellow(X8) | (X8 = X9) | ~ front(X7) | ~ furniture(X7) | ~ seat(X7) | ~ in(X2, X1) | ~ down(X2, X4) | ~ barrel(X2, X3) | ~ lonely(X4) | ~ way(X4) | ~ street(X4) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ event(X2) | ~ city(X1) | ~ hollywood(X1) | ~ front(X0) | ~ furniture(X0) | ~ seat(X0) | ~ in(X19, X17) | ~ (X16 = X19) | ~ in(X18, X10) | ~ (X15 = X18) | ~ young(X16) | ~ man(X16) | ~ fellow(X16) | ~ young(X15) | ~ man(X15) | ~ fellow(X15) | (X15 = X16) | ~ front(X17) | ~ furniture(X17) | ~ seat(X17) | ~ in(X12, X11) | ~ down(X12, X14) | ~ barrel(X12, X13) | ~ lonely(X14) | ~ way(X14) | ~ street(X14) | ~ old(X13) | ~ dirty(X13) | ~ white(X13) | ~ car(X13) | ~ chevy(X13) | ~ event(X12) | ~ city(X11) | ~ hollywood(X11) | ~ front(X10) | ~ furniture(X10) | ~ seat(X10)), inference(equality_resolution, [], [f82])).
fof(f82, plain, ! [X4, X2, X0, X14, X12, X10, X8, X19, X17, X7, X5, X3, X1, X15, X13, X11, X9, X18, X16] : (~ in(X9, X0) | ~ in(X8, X7) | ~ (X5 = X8) | ~ young(X9) | ~ man(X9) | ~ fellow(X9) | ~ young(X5) | ~ man(X5) | ~ fellow(X5) | (X5 = X9) | ~ front(X7) | ~ furniture(X7) | ~ seat(X7) | ~ in(X2, X1) | ~ down(X2, X4) | ~ barrel(X2, X3) | ~ lonely(X4) | ~ way(X4) | ~ street(X4) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ event(X2) | ~ city(X1) | ~ hollywood(X1) | ~ front(X0) | ~ furniture(X0) | ~ seat(X0) | ~ in(X19, X17) | ~ (X16 = X19) | ~ in(X18, X10) | ~ (X15 = X18) | ~ young(X16) | ~ man(X16) | ~ fellow(X16) | ~ young(X15) | ~ man(X15) | ~ fellow(X15) | (X15 = X16) | ~ front(X17) | ~ furniture(X17) | ~ seat(X17) | ~ in(X12, X11) | ~ down(X12, X14) | ~ barrel(X12, X13) | ~ lonely(X14) | ~ way(X14) | ~ street(X14) | ~ old(X13) | ~ dirty(X13) | ~ white(X13) | ~ car(X13) | ~ chevy(X13) | ~ event(X12) | ~ city(X11) | ~ hollywood(X11) | ~ front(X10) | ~ furniture(X10) | ~ seat(X10)), inference(equality_resolution, [], [f81])).
fof(f81, plain, ! [X6, X4, X2, X0, X14, X12, X10, X8, X19, X17, X7, X5, X3, X1, X15, X13, X11, X9, X18, X16] : (~ in(X9, X0) | ~ (X6 = X9) | ~ in(X8, X7) | ~ (X5 = X8) | ~ young(X6) | ~ man(X6) | ~ fellow(X6) | ~ young(X5) | ~ man(X5) | ~ fellow(X5) | (X5 = X6) | ~ front(X7) | ~ furniture(X7) | ~ seat(X7) | ~ in(X2, X1) | ~ down(X2, X4) | ~ barrel(X2, X3) | ~ lonely(X4) | ~ way(X4) | ~ street(X4) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ event(X2) | ~ city(X1) | ~ hollywood(X1) | ~ front(X0) | ~ furniture(X0) | ~ seat(X0) | ~ in(X19, X17) | ~ (X16 = X19) | ~ in(X18, X10) | ~ (X15 = X18) | ~ young(X16) | ~ man(X16) | ~ fellow(X16) | ~ young(X15) | ~ man(X15) | ~ fellow(X15) | (X15 = X16) | ~ front(X17) | ~ furniture(X17) | ~ seat(X17) | ~ in(X12, X11) | ~ down(X12, X14) | ~ barrel(X12, X13) | ~ lonely(X14) | ~ way(X14) | ~ street(X14) | ~ old(X13) | ~ dirty(X13) | ~ white(X13) | ~ car(X13) | ~ chevy(X13) | ~ event(X12) | ~ city(X11) | ~ hollywood(X11) | ~ front(X10) | ~ furniture(X10) | ~ seat(X10)), inference(cnf_transformation, [], [f15])).
fof(f407, plain, (~ spl22_33 | spl22_64), inference(avatar_split_clause, [], [f47, f404, f250])).
fof(f47, plain, (seat(sK12) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((in(sK21, sK12) & (sK18 = sK21) & in(sK20, sK19) & (sK17 = sK20) & young(sK18) & man(sK18) & fellow(sK18) & young(sK17) & man(sK17) & fellow(sK17) & ~ (sK17 = sK18) & front(sK19) & furniture(sK19) & seat(sK19) & in(sK14, sK13) & down(sK14, sK16) & barrel(sK14, sK15) & lonely(sK16) & way(sK16) & street(sK16) & old(sK15) & dirty(sK15) & white(sK15) & car(sK15) & chevy(sK15) & event(sK14) & city(sK13) & hollywood(sK13) & front(sK12) & furniture(sK12) & seat(sK12)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12, sK13, sK14, sK15, sK16, sK17, sK18, sK19, sK20, sK21])], [f12, f13])).
fof(f13, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X0) & (X6 = X9) & in(X8, X7) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0)) => (in(sK21, sK12) & (sK18 = sK21) & in(sK20, sK19) & (sK17 = sK20) & young(sK18) & man(sK18) & fellow(sK18) & young(sK17) & man(sK17) & fellow(sK17) & ~ (sK17 = sK18) & front(sK19) & furniture(sK19) & seat(sK19) & in(sK14, sK13) & down(sK14, sK16) & barrel(sK14, sK15) & lonely(sK16) & way(sK16) & street(sK16) & old(sK15) & dirty(sK15) & white(sK15) & car(sK15) & chevy(sK15) & event(sK14) & city(sK13) & hollywood(sK13) & front(sK12) & furniture(sK12) & seat(sK12))), introduced(choice_axiom, [])).
fof(f12, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X0) & (X6 = X9) & in(X8, X7) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0)) | ~ sP0), inference(rectify, [], [f11])).
fof(f11, plain, (? [X20, X21, X22, X23, X24, X25, X26, X27, X28, X29] : (in(X29, X20) & (X26 = X29) & in(X28, X27) & (X25 = X28) & young(X26) & man(X26) & fellow(X26) & young(X25) & man(X25) & fellow(X25) & ~ (X25 = X26) & front(X27) & furniture(X27) & seat(X27) & in(X22, X21) & down(X22, X24) & barrel(X22, X23) & lonely(X24) & way(X24) & street(X24) & old(X23) & dirty(X23) & white(X23) & car(X23) & chevy(X23) & event(X22) & city(X21) & hollywood(X21) & front(X20) & furniture(X20) & seat(X20)) | ~ sP0), inference(nnf_transformation, [], [f5])).
fof(f402, plain, (~ spl22_33 | spl22_63), inference(avatar_split_clause, [], [f48, f399, f250])).
fof(f48, plain, (furniture(sK12) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f397, plain, (~ spl22_33 | spl22_62), inference(avatar_split_clause, [], [f49, f394, f250])).
fof(f49, plain, (front(sK12) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f392, plain, (~ spl22_33 | spl22_61), inference(avatar_split_clause, [], [f50, f389, f250])).
fof(f50, plain, (hollywood(sK13) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f387, plain, (~ spl22_33 | spl22_60), inference(avatar_split_clause, [], [f51, f384, f250])).
fof(f51, plain, (city(sK13) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f382, plain, (~ spl22_33 | spl22_59), inference(avatar_split_clause, [], [f52, f379, f250])).
fof(f52, plain, (event(sK14) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f377, plain, (~ spl22_33 | spl22_58), inference(avatar_split_clause, [], [f53, f374, f250])).
fof(f53, plain, (chevy(sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f372, plain, (~ spl22_33 | spl22_57), inference(avatar_split_clause, [], [f54, f369, f250])).
fof(f54, plain, (car(sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f367, plain, (~ spl22_33 | spl22_56), inference(avatar_split_clause, [], [f55, f364, f250])).
fof(f55, plain, (white(sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f362, plain, (~ spl22_33 | spl22_55), inference(avatar_split_clause, [], [f56, f359, f250])).
fof(f56, plain, (dirty(sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f357, plain, (~ spl22_33 | spl22_54), inference(avatar_split_clause, [], [f57, f354, f250])).
fof(f57, plain, (old(sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f352, plain, (~ spl22_33 | spl22_53), inference(avatar_split_clause, [], [f58, f349, f250])).
fof(f58, plain, (street(sK16) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f347, plain, (~ spl22_33 | spl22_52), inference(avatar_split_clause, [], [f59, f344, f250])).
fof(f59, plain, (way(sK16) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f342, plain, (~ spl22_33 | spl22_51), inference(avatar_split_clause, [], [f60, f339, f250])).
fof(f60, plain, (lonely(sK16) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f337, plain, (~ spl22_33 | spl22_50), inference(avatar_split_clause, [], [f61, f334, f250])).
fof(f61, plain, (barrel(sK14, sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f332, plain, (~ spl22_33 | spl22_49), inference(avatar_split_clause, [], [f62, f329, f250])).
fof(f62, plain, (down(sK14, sK16) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f327, plain, (~ spl22_33 | spl22_48), inference(avatar_split_clause, [], [f63, f324, f250])).
fof(f63, plain, (in(sK14, sK13) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f322, plain, (~ spl22_33 | spl22_47), inference(avatar_split_clause, [], [f64, f319, f250])).
fof(f64, plain, (seat(sK19) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f317, plain, (~ spl22_33 | spl22_46), inference(avatar_split_clause, [], [f65, f314, f250])).
fof(f65, plain, (furniture(sK19) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f312, plain, (~ spl22_33 | spl22_45), inference(avatar_split_clause, [], [f66, f309, f250])).
fof(f66, plain, (front(sK19) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f307, plain, (~ spl22_33 | ~ spl22_44), inference(avatar_split_clause, [], [f67, f304, f250])).
fof(f67, plain, (~ (sK17 = sK18) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f302, plain, (~ spl22_33 | spl22_43), inference(avatar_split_clause, [], [f68, f299, f250])).
fof(f68, plain, (fellow(sK17) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f297, plain, (~ spl22_33 | spl22_42), inference(avatar_split_clause, [], [f69, f294, f250])).
fof(f69, plain, (man(sK17) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f292, plain, (~ spl22_33 | spl22_41), inference(avatar_split_clause, [], [f70, f289, f250])).
fof(f70, plain, (young(sK17) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f287, plain, (~ spl22_33 | spl22_40), inference(avatar_split_clause, [], [f71, f284, f250])).
fof(f71, plain, (fellow(sK18) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f282, plain, (~ spl22_33 | spl22_39), inference(avatar_split_clause, [], [f72, f279, f250])).
fof(f72, plain, (man(sK18) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f277, plain, (~ spl22_33 | spl22_38), inference(avatar_split_clause, [], [f73, f274, f250])).
fof(f73, plain, (young(sK18) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f272, plain, (~ spl22_33 | spl22_37), inference(avatar_split_clause, [], [f74, f269, f250])).
fof(f74, plain, ((sK17 = sK20) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f267, plain, (~ spl22_33 | spl22_36), inference(avatar_split_clause, [], [f75, f264, f250])).
fof(f75, plain, (in(sK20, sK19) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f262, plain, (~ spl22_33 | spl22_35), inference(avatar_split_clause, [], [f76, f259, f250])).
fof(f76, plain, ((sK18 = sK21) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f257, plain, (~ spl22_33 | spl22_34), inference(avatar_split_clause, [], [f77, f254, f250])).
fof(f77, plain, (in(sK21, sK12) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f248, plain, (~ spl22_1 | spl22_32), inference(avatar_split_clause, [], [f16, f245, f91])).
fof(f16, plain, (seat(sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ((in(sK11, sK9) & (sK8 = sK11) & in(sK10, sK2) & (sK7 = sK10) & young(sK8) & man(sK8) & fellow(sK8) & young(sK7) & man(sK7) & fellow(sK7) & ~ (sK7 = sK8) & front(sK9) & furniture(sK9) & seat(sK9) & in(sK4, sK3) & down(sK4, sK6) & barrel(sK4, sK5) & lonely(sK6) & way(sK6) & street(sK6) & old(sK5) & dirty(sK5) & white(sK5) & car(sK5) & chevy(sK5) & event(sK4) & city(sK3) & hollywood(sK3) & front(sK2) & furniture(sK2) & seat(sK2)) | ~ sP1), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3, sK4, sK5, sK6, sK7, sK8, sK9, sK10, sK11])], [f8, f9])).
fof(f9, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X7) & (X6 = X9) & in(X8, X0) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0)) => (in(sK11, sK9) & (sK8 = sK11) & in(sK10, sK2) & (sK7 = sK10) & young(sK8) & man(sK8) & fellow(sK8) & young(sK7) & man(sK7) & fellow(sK7) & ~ (sK7 = sK8) & front(sK9) & furniture(sK9) & seat(sK9) & in(sK4, sK3) & down(sK4, sK6) & barrel(sK4, sK5) & lonely(sK6) & way(sK6) & street(sK6) & old(sK5) & dirty(sK5) & white(sK5) & car(sK5) & chevy(sK5) & event(sK4) & city(sK3) & hollywood(sK3) & front(sK2) & furniture(sK2) & seat(sK2))), introduced(choice_axiom, [])).
fof(f8, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8, X9] : (in(X9, X7) & (X6 = X9) & in(X8, X0) & (X5 = X8) & young(X6) & man(X6) & fellow(X6) & young(X5) & man(X5) & fellow(X5) & ~ (X5 = X6) & front(X7) & furniture(X7) & seat(X7) & in(X2, X1) & down(X2, X4) & barrel(X2, X3) & lonely(X4) & way(X4) & street(X4) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & event(X2) & city(X1) & hollywood(X1) & front(X0) & furniture(X0) & seat(X0)) | ~ sP1), inference(nnf_transformation, [], [f6])).
fof(f243, plain, (~ spl22_1 | spl22_31), inference(avatar_split_clause, [], [f17, f240, f91])).
fof(f17, plain, (furniture(sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f238, plain, (~ spl22_1 | spl22_30), inference(avatar_split_clause, [], [f18, f235, f91])).
fof(f18, plain, (front(sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f233, plain, (~ spl22_1 | spl22_29), inference(avatar_split_clause, [], [f19, f230, f91])).
fof(f19, plain, (hollywood(sK3) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f228, plain, (~ spl22_1 | spl22_28), inference(avatar_split_clause, [], [f20, f225, f91])).
fof(f20, plain, (city(sK3) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f223, plain, (~ spl22_1 | spl22_27), inference(avatar_split_clause, [], [f21, f220, f91])).
fof(f21, plain, (event(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f218, plain, (~ spl22_1 | spl22_26), inference(avatar_split_clause, [], [f22, f215, f91])).
fof(f22, plain, (chevy(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f213, plain, (~ spl22_1 | spl22_25), inference(avatar_split_clause, [], [f23, f210, f91])).
fof(f23, plain, (car(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f208, plain, (~ spl22_1 | spl22_24), inference(avatar_split_clause, [], [f24, f205, f91])).
fof(f24, plain, (white(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f203, plain, (~ spl22_1 | spl22_23), inference(avatar_split_clause, [], [f25, f200, f91])).
fof(f25, plain, (dirty(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f198, plain, (~ spl22_1 | spl22_22), inference(avatar_split_clause, [], [f26, f195, f91])).
fof(f26, plain, (old(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f193, plain, (~ spl22_1 | spl22_21), inference(avatar_split_clause, [], [f27, f190, f91])).
fof(f27, plain, (street(sK6) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f188, plain, (~ spl22_1 | spl22_20), inference(avatar_split_clause, [], [f28, f185, f91])).
fof(f28, plain, (way(sK6) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f183, plain, (~ spl22_1 | spl22_19), inference(avatar_split_clause, [], [f29, f180, f91])).
fof(f29, plain, (lonely(sK6) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f178, plain, (~ spl22_1 | spl22_18), inference(avatar_split_clause, [], [f30, f175, f91])).
fof(f30, plain, (barrel(sK4, sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f173, plain, (~ spl22_1 | spl22_17), inference(avatar_split_clause, [], [f31, f170, f91])).
fof(f31, plain, (down(sK4, sK6) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f168, plain, (~ spl22_1 | spl22_16), inference(avatar_split_clause, [], [f32, f165, f91])).
fof(f32, plain, (in(sK4, sK3) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f163, plain, (~ spl22_1 | spl22_15), inference(avatar_split_clause, [], [f33, f160, f91])).
fof(f33, plain, (seat(sK9) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f158, plain, (~ spl22_1 | spl22_14), inference(avatar_split_clause, [], [f34, f155, f91])).
fof(f34, plain, (furniture(sK9) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f153, plain, (~ spl22_1 | spl22_13), inference(avatar_split_clause, [], [f35, f150, f91])).
fof(f35, plain, (front(sK9) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f148, plain, (~ spl22_1 | ~ spl22_12), inference(avatar_split_clause, [], [f36, f145, f91])).
fof(f36, plain, (~ (sK7 = sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f143, plain, (~ spl22_1 | spl22_11), inference(avatar_split_clause, [], [f37, f140, f91])).
fof(f37, plain, (fellow(sK7) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f138, plain, (~ spl22_1 | spl22_10), inference(avatar_split_clause, [], [f38, f135, f91])).
fof(f38, plain, (man(sK7) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f133, plain, (~ spl22_1 | spl22_9), inference(avatar_split_clause, [], [f39, f130, f91])).
fof(f39, plain, (young(sK7) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f128, plain, (~ spl22_1 | spl22_8), inference(avatar_split_clause, [], [f40, f125, f91])).
fof(f40, plain, (fellow(sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f123, plain, (~ spl22_1 | spl22_7), inference(avatar_split_clause, [], [f41, f120, f91])).
fof(f41, plain, (man(sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f118, plain, (~ spl22_1 | spl22_6), inference(avatar_split_clause, [], [f42, f115, f91])).
fof(f42, plain, (young(sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f113, plain, (~ spl22_1 | spl22_5), inference(avatar_split_clause, [], [f43, f110, f91])).
fof(f43, plain, ((sK7 = sK10) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f108, plain, (~ spl22_1 | spl22_4), inference(avatar_split_clause, [], [f44, f105, f91])).
fof(f44, plain, (in(sK10, sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f103, plain, (~ spl22_1 | spl22_3), inference(avatar_split_clause, [], [f45, f100, f91])).
fof(f45, plain, ((sK8 = sK11) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f98, plain, (~ spl22_1 | spl22_2), inference(avatar_split_clause, [], [f46, f95, f91])).
fof(f46, plain, (in(sK11, sK9) | ~ sP1), inference(cnf_transformation, [], [f10])).