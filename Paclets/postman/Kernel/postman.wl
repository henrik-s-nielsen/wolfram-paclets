(* ::Package:: *)

BeginPackage["postman`"];


postmanCollectionImport;
postmanCollectionImportByteArray;
postmanTemplateSlots;


Begin["`Private`"];


postmanCollectionImport[file_String] := postmanCollectionImportByteArray@ReadByteArray@file;
postmanCollectionImportByteArray[collection_ByteArray] := Module[
	{data, importGroup, importItem, importRequest,strTemplate,importAuth},
	importItem[item_Association] := <|
		"name"->item["name"],
		If[KeyExistsQ[item,"item"], "item"->importItem /@ item["item"],Nothing],
		If[KeyExistsQ[item,"request"],"request"->importRequest@item["request"],Nothing]
	|>;
	importAuth[auth_Association] := Module[
		{authAssociation=<|#key->#value& /@ auth["basic"]|>},
		With[{
			username = strTemplate@authAssociation["username"],
			password = strTemplate@authAssociation["password"]},
		TemplateExpression[username<>":"<>password]]
	];
	strTemplate[str_String] := StringTemplate@StringReplace[str,{"{{"->"`","}}"->"`"}] /; 
		StringContainsQ[str,"{{"];
	strTemplate[str_String] := str;
	importRequest[req_Association] := TemplateObject[HTTPRequest[
		strTemplate@req["url","raw"],<|
			Method->req["method"],
			"Headers"->If[KeyExistsQ[req,"auth"],{"Authorization"->importAuth[req["auth"]]},{}]~Join~
			(#key->#value&/@(req["header"]/._Missing->{})),
			If[KeyExistsQ[req,"body"],"Body" -> req["body","raw"],Nothing]
		|>]];
	
	data=ImportByteArray[collection,"RawJSON"];
	TemplateObject[<|
		"info"->data["info"],
		"item" -> importItem /@ data["item"]
	|>]
];


postmanTemplateSlots[template_] := Module[{slots},
	slots = (Cases[template,_TemplateSlot,Infinity] // DeleteDuplicates) /. TemplateSlot[s_] :> s;
	<| #-> Global`\[Placeholder] & /@ slots |>
]


End[];


EndPackage[]
