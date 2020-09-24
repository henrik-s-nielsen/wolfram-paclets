(* ::Package:: *)

BeginPackage["curl`"];


curlExport;
curlExportVariables;


Begin["`Private`"];


templateSlots[template_] := Module[{slots},
	slots = (Cases[template,_TemplateSlot,Infinity] // DeleteDuplicates) /. TemplateSlot[s_] :> s;
	<| #-> ("$"<>#) & /@ slots |>
]


exportMethod[method_String] := "-X " <> method;
exportContentType[type_String] := "-H \"Content-Type: " <> type <>"\"";
exportContentType[None] := Nothing;
exportHeader["user-agent" -> value_String] := Nothing /; StringContainsQ[value,"Wolfram"]; 
exportHeader[key_ -> value_] := StringTemplate["-H \"``: ``\""][key,value];
exportHeaders[headers_List] := StringRiffle[exportHeader/@headers," "];
exportBody[body_String] := StringTemplate["--data '``'"][body];
exportBody[""] := Nothing;
exportRequest[req_HTTPRequest] := 
	StringRiffle[{
		"curl",
		"-L",
		exportHeaders@req["Headers"],
		exportMethod@req["Method"],
		exportBody@req["Body"],
		"'"<>req["URL"]<>"'"
	}, " "];

	
curlExport[req_HTTPRequest] := Module[{templateReq},
	templateReq = TemplateObject[req];
	templateReq = Replace[templateReq,TemplateSlot[var_String] :> "$" <> var, Infinity];
	exportRequest@templateReq[]
];



curlExportVariables[data_Association] := 
	StringRiffle[KeyValueMap[
		Function[{key,value},StringTemplate["export ``=``"][key,exportVariableValue@value]],
		data
	]," \\\n"];
exportVariableValue[s_String] := StringTemplate["\"``\""][s];
exportVariableValue[v_] := v;


End[];


EndPackage[]
