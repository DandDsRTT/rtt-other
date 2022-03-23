increment = 0.01;
generatorSizes = Reverse[Range[increment,1-increment,increment]];
Print["generatorSizes: ", generatorSizes];
edos = Map[1200/#&,generatorSizes];
Print["edos: ", edos];
uniformMaps = Map[getUniformMap[#,2]&, edos];
approxesOfTwo = MapThread[#1*#2&, {uniformMaps, generatorSizes}];
errors = MapIndexed[{First[#2],First[#1]-1200}&,approxesOfTwo];


ListPlot[errors, ImageSize -> 1500]
