(* ::Package:: *)

BeginPackage["SHPExport`"]

exportshapefile::usage = "exportshapefile[filepath_, geometry_, assoc_] exports an ESRI shapefile and related files.";

Begin["`Private`"]

writeshp[geometry_, filepath_] := 
 Module[{str = OpenWrite[filepath, BinaryFormat -> True], 
   shx = OpenWrite[StringReplace[filepath, ".shp" -> ".shx"], 
     BinaryFormat -> True], shapetype, bounds, recordnumber = 0},
  BinaryWrite[str, {9994, 0, 0, 0, 0, 0, filelength[geometry]}, 
   "Integer32", ByteOrdering -> 1];
  shapetype = 
   Pick[{1, 3, 5}, {Point, Line, Polygon}, 
     Commonest[geometry[[All, 0]]][[1]]][[1]];
  BinaryWrite[str, {1000, shapetype}, "Integer32", ByteOrdering -> -1];
  bounds = 
   If[shapetype == 1, MinMax /@ Transpose[geometry[[All, 1]]], 
    MinMax /@ Transpose[Join @@ (geometry[[All, 1]])]];
  BinaryWrite[
   str, {bounds[[1, 1]], bounds[[2, 1]], bounds[[1, 2]], 
    bounds[[2, 2]], 0., 0., 0., 0.}, "Real64", ByteOrdering -> -1];
  BinaryWrite[shx, {9994, 0, 0, 0, 0, 0, (100 + 8*Length@geometry)/2},
    "Integer32", ByteOrdering -> 1];
  BinaryWrite[shx, {1000, shapetype}, "Integer32", ByteOrdering -> -1];
  BinaryWrite[
   shx, {bounds[[1, 1]], bounds[[2, 1]], bounds[[1, 2]], 
    bounds[[2, 2]], 0., 0., 0., 0.}, "Real64", ByteOrdering -> -1];
  Which[
   shapetype == 1,
   Do[writepoint[str, shx, record, recordnumber++], {record, 
     geometry}],
   shapetype == 3,
   Do[writepolyline[str, shx, record, recordnumber++], {record, 
     geometry}],
   shapetype == 5,
   Do[writepolygon[str, shx, record, recordnumber++], {record, 
     geometry}]
   ];
  Close[str];
  Close[shx];
  ]

writepolyline[stream_, shxstream_, linerecord_, recordnumber_] := 
 Module[{numpart = Depth@linerecord[[1]] - 2, numpoints, bounds},
  If[numpart > 1, numpoints = Total[Length /@ linerecord[[1]]]; 
   bounds = MinMax /@ Transpose[Join @@ (linerecord[[1]])], 
   numpoints = Length@linerecord[[1]]; 
   bounds = MinMax /@ Transpose[linerecord[[1]]]];
  BinaryWrite[
   shxstream, {StreamPosition[stream]/2, 
    22 + 2*numpart + 8*numpoints}, "Integer32", ByteOrdering -> 1];
  BinaryWrite[stream, {recordnumber, 22 + 2*numpart + 8*numpoints}, 
   "Integer32", ByteOrdering -> 1];
  BinaryWrite[
   stream, {3, bounds[[1, 1]], bounds[[2, 1]], bounds[[1, 2]], 
    bounds[[2, 2]], numpart, numpoints, 
    Sequence @@ Range[0, numpart - 1], 
    Sequence @@ (Flatten@linerecord[[1]])}, {"Integer32", "Real64", 
    "Real64", "Real64", "Real64", "Integer32", "Integer32", 
    Sequence @@ ConstantArray["Integer32", numpart], 
    Sequence @@ ConstantArray["Real64", numpoints*2]}, 
   ByteOrdering -> -1]
  ]

writepoint[stream_, shxstream_, pointrecord_, recordnumber_] := 
 Module[{numpart = Depth@pointrecord[[1]] - 2, numpoints, bounds},
  BinaryWrite[shxstream, {StreamPosition[stream]/2, 10}, "Integer32", 
   ByteOrdering -> 1];
  BinaryWrite[stream, {recordnumber, 10}, "Integer32", 
   ByteOrdering -> 1];
  BinaryWrite[
   stream, {1, Sequence @@ (Flatten@pointrecord[[1]])}, {"Integer32", 
    "Real64", "Real64"}, ByteOrdering -> -1]
  ]

writepolygon[stream_, shxstream_, polyrecord_, recordnumber_] := 
 Module[{numpart = Depth@polyrecord[[1]] - 2, numpoints, bounds},
  If[numpart > 1, numpoints = Total[Length /@ polyrecord[[1]]]; 
   bounds = MinMax /@ Transpose[Join @@ (polyrecord[[1]])], 
   numpoints = Length@polyrecord[[1]]; 
   bounds = MinMax /@ Transpose[polyrecord[[1]]]];
  BinaryWrite[
   shxstream, {StreamPosition[stream]/2, 
    22 + 2*numpart + 8*numpoints}, "Integer32", ByteOrdering -> 1];
  BinaryWrite[stream, {recordnumber, 22 + 2*numpart + 8*numpoints}, 
   "Integer32", ByteOrdering -> 1];
  BinaryWrite[
   stream, {5, bounds[[1, 1]], bounds[[2, 1]], bounds[[1, 2]], 
    bounds[[2, 2]], numpart, numpoints, 
    Sequence @@ Range[0, numpart - 1], 
    Sequence @@ (Flatten@polyrecord[[1]])}, {"Integer32", "Real64", 
    "Real64", "Real64", "Real64", "Integer32", "Integer32", 
    Sequence @@ ConstantArray["Integer32", numpart], 
    Sequence @@ ConstantArray["Real64", numpoints*2]}, 
   ByteOrdering -> -1]
  ]

filelength[geometry_] := 
 Module[{shapetype = 
    Pick[{1, 3, 5}, {Point, Line, Polygon}, 
      Commonest[geometry[[All, 0]]][[1]]][[1]], 
   records = Length@geometry[[All, 1]], 
   points = Length@Flatten@geometry[[All, 1]], 
   parts = Total[Depth /@ geometry[[All, 1]] - 2]},
  If[shapetype == 1, 28*records + 100, 
    52*records + 4*parts + 8*points + 100]/2
  ]

writedbf[assoc_, filepath_] := 
 Module[{str = OpenWrite[filepath, BinaryFormat -> True], 
   vals = Values@assoc, fieldnames = Keys[assoc], fieldtypes, offsets,
    header, subrecords, recods, recordstart, records},
  fieldnames = 
   If[StringLength[#] > 10, StringTake[#, 10], #] & /@ fieldnames;
  fieldtypes = Commonest[Head /@ #][[1]] & /@ vals;
  Do[If[fieldtypes[[i]] == Real, 
    vals[[i]] = realformat[vals[[i]]]], {i, Length@vals}];
  offsets = 
   Table[Switch[fieldtypes[[i]], 
     String, {Max[50, Max[Length /@ vals[[i]]]], 0}, Integer, {16, 0},
      Real, {19, 11}], {i, Length@vals}];
  fieldtypes = 
   Pick[{"C", "N", "F"}, {String, Integer, Real}, #][[1]] & /@ 
    fieldtypes;
  subrecords = 
   Flatten@Table[
     Flatten[{PadRight[ToCharacterCode[fieldnames[[i]]], 11, 0], 
       ToCharacterCode[fieldtypes[[i]]], 0, 0, 0, 0, 
       Sequence @@ offsets[[i]], 0, ConstantArray[0, 13]}], {i, 
      Length@fieldnames}];
  recordstart = 32*(Length@vals + 1) + 1;
  records = 
   Transpose@
    Table[If[fieldtypes[[i]] == "N", PadLeft, PadRight][
      ToCharacterCode[ToString[vals[[i, j]]]], offsets[[i, 1]], 
      32], {i, Length@vals}, {j, Length@vals[[i]]}];
  header = {3, DateList[][[1]] - 1900, DateList[][[2]], 
    DateList[][[3]], Length@vals[[1]], recordstart, 
    Length@Flatten@records[[1]] + 1, Sequence @@ ConstantArray[0, 17],
     87, 0, 0};
  BinaryWrite[str, 
   header, {Sequence @@ ConstantArray["Byte", 4], "Integer32", 
    "Integer16", "Integer16", Sequence @@ ConstantArray["Byte", 20]}];
  BinaryWrite[str, subrecords, "Byte"];
  BinaryWrite[str, {13}, "Byte"];
  BinaryWrite[str, Flatten[Prepend[#, 32] & /@ (Join @@@ records)], 
   "Byte"];
  BinaryWrite[str, {26}, "Byte"];
  Close[str];
  ]

realformat[num_] := 
 ToString@ScientificForm@PaddedForm[num, {12, 11}, 
  NumberFormat -> (Row[{#1, "e", If[ToExpression[(#3 /. "" -> "0")] < 0, "-", "+"], StringPadLeft[StringReplace[ToString@#3, "-" -> ""], 3,  "0"]}] &)]
SetAttributes[realformat, Listable]

exportshapefile[filepath_, geometry_, assoc_] := Module[{},
  If[! StringMatchQ[filepath, __ ~~ "SHP", IgnoreCase -> True], 
   Abort[]];
  If[Length@First@Values@assoc != Length@geometry, Abort[]];
  writeshp[geometry, filepath];
  writedbf[assoc, StringReplace[filepath, ".shp" -> ".dbf"]];
  filepath
  ]

End[]
EndPackage[]






