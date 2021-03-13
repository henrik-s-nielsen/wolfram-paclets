(* ::Package:: *)

BeginPackage["har`"];


importHAR;
importHARString
importHARByteArray;


Begin["`Private`"];


importHAR[file_String] := 
	importHARByteArray@ReadByteArray@file;
importHARString[har_ByteArray] := 
	importHARByteArray@StringToByteArray[har];
importHARByteArray[har_ByteArray] := Module[{data},
	data = ImportByteArray[har, "RawJSON"];
	<|
		"Version" -> "",
		"Creator" -> "",
		"Entries" -> importEntry /@ data["entries"]
	|>
];



impOptKey[data_Association,key1_,key2_]:=
	impOptKey[data, key1, key2, Identity];
impOptKey[data_Association,key1_,key2_, f_]:=
	If[KeyExistsQ[data, key1], key2 -> f@data["key1"], Nothing];
importHAR[harTxt_String] := Module[{jsonLog},
	jsonLog = ImportString[harTxt, "RawJSON"];
	Dataset[importEntry /@ jsonLog["log", "entries"]]
];
importEntry[entry_Association]:= Module[{started, totalTime},
	started = ResourceFunction["FromISOTimestamp"][entry["startedDateTime"]];
	totalTime = Quantity[entry["time"], "Milliseconds"];
	<|
		"Started" -> started,
		"Ended" -> started + totalTime,
		"Time" -> totalTime, 
		"Request" -> importRequest[entry["request"]],
		"Response" -> importResponse[entry["response"]],
		"Raw" -> Iconize[entry]
	|>
];
normalizeRequestHeaders[headers_List] :=
	headers/.("Host"->_)->Nothing;
importHeader[KeyValuePattern[{"name"->name_String, "value"->value_}]] :=
	name->value;
importCookie[c_Association] := <|
	"Domain" -> c["domain"],
	"Name" -> c["name"],
	impOptKey[c,"path","Path"],
	impOptKey[c,"value","Content"],
	"ExpirationDate" -> impOptKey[c, "expires", "ExpirationDate",
		ResourceFunction["FromISOTimestamp"]]
|>;
importPostData[req_Association] := If[KeyExistsQ[req, "postData"],
	"Body" -> req["postData", "text"],
	Nothing
];
importContentType[req_Association] := If[KeyExistsQ[req, "postData"],
	"ContentType" -> req["postData", "mimeType"],
	Nothing
];
importRequest[req_Association] :=
	HTTPRequest[req["url"], <|
		Method -> req["method"],
		importContentType[req],
		"Headers"->((importHeader /@ req["headers"]) // normalizeRequestHeaders),
		"Cookies"->importHeader /@ req["cookies"],
		importPostData[req]
	|>
];
decodeContent["base64", txt_String] := BaseDecode[txt];
importContent[content_Association] := If[KeyExistsQ[content,"text"],
	If[KeyExistsQ[content,"encoding"],
		decodeContent[content["encoding"],content["text"]],
		content["text"]],
	None
];
importResponse[res_Association] :=
	HTTPResponse[
		importContent[res["content"]],
		<|
			"ContentType" -> res["content","mimeType"],
			"StatusCode" -> res["status"]
		|>
	];


EndPackage[]
