failures = 0;
passes = 0;

m = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* meantone *)

test[optimizeGtm, m, \[Infinity], {1200., 696.578}];

test[optimizeGtm, m, \[Infinity], True, "regressive", "F", 1, {1202.39, 697.176}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "F", 2, {1202.61, 696.741}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "P", 1, {1201.7, 697.564}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "P", 2, {1201.4, 697.049}];

test[optimizeGtm, m, \[Infinity], True, "regressive", "F", {1202.39, 697.176}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "F", 2, {1202.61, 696.741}];
test[optimizeGtm, m, \[Infinity], True, {1201.7, 697.564}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "P", 2, {1201.4, 697.049}];

test[optimizeGtm, m, \[Infinity], True, "progressive", "F", {1197.61, 694.786}];
test[optimizeGtm, m, \[Infinity], True, "progressive", "F", 2, {1197.43, 694.976}];
test[optimizeGtm, m, \[Infinity], True, "progressive", {1197.98, 694.711}];
test[optimizeGtm, m, \[Infinity], True, "progressive", "P", 2, {1198.16, 695.01}];


test[optimizeGtm, m, 2, {1199.02, 695.601}];

test[optimizeGtm, m, 2, True, "regressive", "F", {1200.07, 696.005}];
test[optimizeGtm, m, 2, True, "regressive", "F", 2, {1200.74, 696.205}];
test[optimizeGtm, m, 2, True, {1200.98, 696.904}];
test[optimizeGtm, m, 2, True, "regressive", "P", 2, {1201.13, 696.905}];

test[optimizeGtm, m, 2, True, "progressive", "F", {1198.4, 695.289}];
test[optimizeGtm, m, 2, True, "progressive", "F", 2, {1198.24, 695.294}];
test[optimizeGtm, m, 2, True, "progressive", {1198.08, 694.93}];
test[optimizeGtm, m, 2, True, "progressive", "P", 2, {1197.93, 694.911}];


test[optimizeGtm, m, 1, {1200., 696.578}];

test[optimizeGtm, m, 1, True, "regressive", "F", {1195.7, 693.352}];
test[optimizeGtm, m, 1, True, "regressive", "F", 2, {1195.7, 693.352}];
test[optimizeGtm, m, 1, True, {1200., 696.578}];
test[optimizeGtm, m, 1, True, "regressive", "P", 2, {1200., 696.578}];

test[optimizeGtm, m, 1, True, "progressive", "F", {1200., 696.578}];
test[optimizeGtm, m, 1, True, "progressive", "F", 2, {1200., 696.578}];
test[optimizeGtm, m, 1, True, "progressive", {1195.7, 693.352}];
test[optimizeGtm, m, 1, True, "progressive", "P", 2, {1195.7, 693.352}];


m = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)

test[optimizeGtm, m, \[Infinity], SetPrecision[{600., 106.84},4]];

test[optimizeGtm, m, \[Infinity], True, "regressive", "F", 1, {597.119, 103.293}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "F", 2, {598.345, 106.693}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "P", 1, {598.447, 106.567}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "P", 2, {598.859, 106.844}];

test[optimizeGtm, m, \[Infinity], True, "regressive", "F", {597.119, 103.293}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "F", 2, {598.345, 106.693}];
test[optimizeGtm, m, \[Infinity], True, {598.447, 106.567}];
test[optimizeGtm, m, \[Infinity], True, "regressive", "P", 2, {598.859, 106.844}];

test[optimizeGtm, m, \[Infinity], True, "progressive", "F", {601.897, 108.014}];
test[optimizeGtm, m, \[Infinity], True, "progressive", "F", 2, {601.99, 108.325}];
test[optimizeGtm, m, \[Infinity], True, "progressive", {601.553, 107.922}];
test[optimizeGtm, m, \[Infinity], True, "progressive", "P", 2, {600.318, 108.159}];


test[optimizeGtm, m, 2, {599.45, 107.15}];

test[optimizeGtm, m, 2, True, "regressive", "F", {597.851, 106.643}];
test[optimizeGtm, m, 2, True, "regressive", "F", 2, {598.31, 106.798}];
test[optimizeGtm, m, 2, True, {598.436, 106.672}];
test[optimizeGtm, m, 2, True, "regressive", "P", 2, {598.762, 106.835}];

test[optimizeGtm, m, 2, True, "progressive", "F", {601.653, 107.288}];
test[optimizeGtm, m, 2, True, "progressive", "F", 2, {601.522, 107.178}];
test[optimizeGtm, m, 2, True, "progressive", {600.655, 107.426}];
test[optimizeGtm, m, 2, True, "progressive", "P", 2, {600.263, 107.259}];


test[optimizeGtm, m, 1, {600., 106.843}];

test[optimizeGtm, m, 1, True, "regressive", "F", {596.741, 105.214}];
test[optimizeGtm, m, 1, True, "regressive", "F", 2, {596.741, 105.214}];
test[optimizeGtm, m, 1, True, {596.741, 105.214}];
test[optimizeGtm, m, 1, True, "regressive", "P", 2, {596.741, 105.214}];

test[optimizeGtm, m, 1, True, "progressive", "F", {601.397, 106.145}];
test[optimizeGtm, m, 1, True, "progressive", "F", 2, {601.397, 106.145}];
test[optimizeGtm, m, 1, True, "progressive", {600., 106.843}];
test[optimizeGtm, m, 1, True, "progressive", "P", 2, {600., 106.843}];

Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
