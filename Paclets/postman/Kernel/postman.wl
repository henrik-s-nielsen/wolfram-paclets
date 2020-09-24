(* ::Package:: *)

BeginPackage["postman`"];


postmanCollectionImport;
postmanCollectionImportByteArray;
postmanTemplateSlots;


Begin["`Private`"];


importBody[KeyValuePattern["body"->KeyValuePattern["raw"->raw_String]]] := "Body" -> strTemplate@raw;
importBody[KeyValuePattern["body"->KeyValuePattern["urlencoded"->data_List]]] := With[
	{dataList = 
		(# /. KeyValuePattern[{"key" -> key_, "value" -> value_}] :> (key -> strTemplate@value)) & /@ data
	},
	"Body" -> 
		TemplateExpression@StringReplace[URLQueryEncode@dataList, "%24"->"$"]
];
importBody[_] := Nothing;

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

importHeaders[req_] :=
	If[req["body","mode"]==="urlencoded",{"content-type"-> "application/x-www-form-urlencoded"},{}] ~Join~
	If[KeyExistsQ[req,"auth"],{"Authorization"->importAuth[req["auth"]]},{}] ~Join~
		(#key -> strTemplate@#value & /@( req["header"] /._Missing->{}))

strTemplate[str_String] := StringTemplate@StringReplace[str,{"{{"->"`","}}"->"`"}] /; 
	StringContainsQ[str,"{{"];
strTemplate[str_String] := str;

importRequest[req_Association] := TemplateObject[HTTPRequest[
	strTemplate@req["url","raw"],<|
		Method->req["method"],
		"Headers"-> importHeaders[req],
		importBody[req]
|>]];

postmanCollectionImport[file_String] := postmanCollectionImportByteArray@ReadByteArray@file;
postmanCollectionImportByteArray[collection_ByteArray] := Module[
	{data},
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
