(* ::Package:: *)

(* ::Title:: *)
(*Confluence*)


(* ::Section:: *)
(*Documentation*)


(* ::Item:: *)
(*Confluence REST API >>*)


(* ::Section:: *)
(*Header*)


BeginPackage["confluence`"];


cnflContentRef;
cnflHttpGet;
cnflHttpPost;
cnflHttpPut;
cnflHttpGetPaged;
cnflGetContent;
cnflUpdate;
cnflInfo;
cnflContentSearch;
cnflContentSearchList;
cnflContentSearchCount;
cnflCreatePage;
cnflOpenUi;


Begin["`Private`"];


(* ::Section:: *)
(*Main*)


(* ::Subsection::Closed:: *)
(*Common*)


contentExpandableTypes={"history","body","container","ancestors","children","descendants","space","version","metadata"};
expandAll="expand="<>StringRiffle[contentExpandableTypes,","];
apiBase="https://santapedia:443";
uiBase="http://santapedia:80";

refType["page"]:="confluence.content.page";
refType["comment"]:="confluence.content.comment";
refType["attachment"]:="confluence.content.attachment";
refType["blogpost"]:="confluence.content.blogpost";

NotebookImport[DirectoryName@FindFile@"confluence`"<>"icons.nb","Input"->"Expression"];
icons = Image[ImageResize[#,15],ImageSize -> 15]& /@ azIcons;

cnflContentRef /: Normal[cnflContentRef[refData_]] := refData; 

cnflContentRef[refData_][prop_String] := refData[prop];

cnflOpenUi[ref_cnflContentRef] := SystemOpen[uiBase <> ref["uiUrl"]];


(* ::Subsection::Closed:: *)
(*Icon panel*)


cnflContentRef /: MakeBoxes[ref:cnflContentRef[refData_Association], fmt_] :=
Block[{type, typeLabel, bg, display, panelInf},
	bg = White;
	type = refData["refType"];
	display = Tooltip[
		Framed[
			Row[{icons[type]," ",Style[Last@StringSplit[type,"."], Italic], ": ", Style[refData["title"],Bold]}],
			Background -> bg,
			BaselinePosition -> Baseline,
			BaseStyle -> "Panel",
			FrameMargins -> {{5,5},{3,3}},
			FrameStyle -> GrayLevel[0.8],
			RoundingRadius -> 5
		],
		Grid[KeyValueMap[{Style[#1,Bold],#2}&,refData],Alignment->Left]
	];
	
	(* should probably recast this as a TemplateBox eventually *)
	With[{boxes = ToBoxes[display, fmt]},
		InterpretationBox[boxes, ref,Editable->False,Selectable->False]
	]
];


(* ::Subsection::Closed:: *)
(*Paging*)


pageData[take_, auth_String, r_HTTPResponse] := r;
pageData[take_, auth_String, ds_Dataset] := pageData[take, auth, Normal@ds];
pageData[take_Integer, auth_String,data_Association] := Module[
	{len},
	len = Length@data["results"];
	PrintTemporary[StringTemplate["Transfering: `` of ``"][take,data["totalSize"]]];
	data["results"] ~ Join ~ If[
		take - len > 0,
		pageData[take -len ,auth, cnflHttpGet[auth,data["_links","next"]]],
		{}
	]
] /; MatchQ[data["_links","next"],_String]
pageData[take_,auth_String,data_Association]:= data["results"] /;
	MatchQ[data["_links","next"],_Missing];



(* ::Subsection:: *)
(*HTTP operations*)


cnflHttpGetPaged[auth_ ,args___] := cnflHttpGetPaged[1000, auth ,args];
cnflHttpGetPaged[take_Integer, auth_ ,args___] := cnflHttpGet[auth, args] // (pageData[take,auth, #] /. l_List :> Dataset@l) & 


cnflHttpGet[auth_String, path_String] := cnflHttpGet[auth, {apiBase<>path}];
cnflHttpGet[auth_String, {url_String}] := Module[{req,res},
	req=HTTPRequest[url, <|"Headers"->{
		"Authorization" -> auth
	}|>];
	Sow[req];
	res=URLRead@req;
	Sow[res];
	res /.(r: HTTPResponse[_,KeyValuePattern["StatusCode"->200],___]:>
		(ImportByteArray[r["BodyByteArray"],"RawJSON"] // Dataset))
];


cnflHttpPost[auth_String, path_String, body_String] := cnflHttpPost[auth, {apiBase<>path}, body];
cnflHttpPost[auth_String, {url_String}, body_String] := Module[{req,res},
	req=HTTPRequest[url, <|Method->"POST",
		"Headers"->{
			"Authorization" -> auth,
			"Content-Type" -> "application/json"
		},
		"Body" -> body
	|>];
	Sow[req];
	res=URLRead@req;
	Sow[res];
	res /.(r: HTTPResponse[_,KeyValuePattern["StatusCode"->(200|201)],___]:>
		(ImportByteArray[r["BodyByteArray"],"RawJSON"] // Dataset))
];


cnflHttpPut[auth_String, path_String, body_String] := cnflHttpPut[auth, {apiBase<>path}, body];
cnflHttpPut[auth_String, {url_String}, body_String] := Module[{req,res},
	req=HTTPRequest[url, <|Method->"PUT",
		"Headers"->{
			"Authorization" -> auth,
			"Content-Type" -> "application/json"
		},
		"Body" -> body
	|>];
	Sow[req];
	res=URLRead@req;
	Sow[res];
	res /.(r: HTTPResponse[_,KeyValuePattern["StatusCode"->(200|201)],___]:>
		(ImportByteArray[r["BodyByteArray"],"RawJSON"] // Dataset))
];


(* ::Subsection::Closed:: *)
(*Creation templates*)


pageTemplate = TemplateObject[<|
	"type" -> "page",
	"title" -> TemplateSlot["title"],
	"ancestors" -> {<|"id" -> TemplateSlot["parentPageId"]|>}, 
	"space" -> <|"key" -> TemplateSlot["spaceKey"]|>,
	"body" ->  <|
		"storage"-><|
			"value"-> TemplateSlot["pageHtml"],
			"representation"->"storage"
		|>
	|>
|>];


(* ::Subsection:: *)
(*uiOpen*)


(* ::Subsection:: *)
(*Info*)


cnflInfo[auth_, ref_cnflContentRef] := cnflGetContent[auth, ref["contentId"]];

cnflGetContent[auth_, id_Integer] := cnflHttpGet[auth,StringTemplate["/rest/api/content/``/?``"][id,expandAll<>URLEncode[",body.storage"]]];


(* ::Subsection::Closed:: *)
(*Search*)


cnflContentSearchCount[auth_, query_String] := cnflHttpGet[auth,StringTemplate["/rest/api/content/search?cql=``&``"][URLEncode@query,expandAll]] /.
	ds_Dataset :> ds["totalSize"];
cnflContentSearch[auth_, args___] := cnflContentSearchList[auth, args] /. ds_Dataset :> Normal@ds[All, "cnflRef"];
cnflContentSearchList[auth_, query_String] := cnflContentSearchList[auth, query, 500];
cnflContentSearchList[auth_, query_String, take_Integer] := cnflHttpGetPaged[take, auth,StringTemplate["/rest/api/content/search?cql=``&``"][URLEncode@query,expandAll]] /.
	ds_Dataset :> ds[All, <|
		"cnflRef" -> cnflContentRef[<|
			"refType" -> refType[#["type"]],
			"contentId" -> ToExpression[#["id"]],
			"title" -> #["title"],
			"spaceKey" ->#["space","key"],
			"uiUrl" ->  #["_links","webui"]
		|>],#|>&];


(* ::Subsection:: *)
(*Create*)


cnflCreatePage[auth_, data:KeyValuePattern[{
	"title"->_String,
	"parentPageId" -> _,
	"spaceKey" -> _String,
	"pageHtml" -> _String	
}]] := cnflHttpPost[
	auth,
	"/rest/api/content",
	ExportString[pageTemplate[data],"RawJSON"]
];


(* ::Subsection:: *)
(*Update*)


cnflUpdate[auth_, ref_cnflContentRef, data_Association] := cnflHttpPut[
	auth,
	"/rest/api/content/"<>ToString@ref["contentId"],
	ExportString[data,"RawJSON"]
];


(* ::Section::Closed:: *)
(*Footer*)


End[];


EndPackage[]
