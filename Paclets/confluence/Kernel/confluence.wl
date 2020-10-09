(* ::Package:: *)

(* ::Title:: *)
(*Confluence*)


(* ::Section:: *)
(*Documentation*)


(* ::Item:: *)
(*Confluence REST API >>*)


(* ::Section::Closed:: *)
(*Header*)


BeginPackage["confluence`"];


cfContentRef;
cfHttpGet;
cfHttpPost;
cfHttpPut;
cfHttpGetPaged;
cfGetContent;
cfInfo;
cfContentSearch;
cfContentSearchList;
cfContentSearchCount;
cfCreatePage;
cfOpenUi;
cfLastVersion;
cfUpdatePageBody;


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Base*)


(* ::Subsection::Closed:: *)
(*Utilities*)


contentExpandableTypes={"history","body","container","ancestors","children","descendants","space","version","metadata"};
expandAll="expand="<>StringRiffle[contentExpandableTypes,","];
apiBase="https://santapedia:443";
uiBase="http://santapedia:80";

refType["page"]:="confluence.content.page";
refType["comment"]:="confluence.content.comment";
refType["attachment"]:="confluence.content.attachment";
refType["blogpost"]:="confluence.content.blogpost";

NotebookImport[DirectoryName@FindFile@"confluence`"<>"icons.nb","Input"->"Expression"];
icons = Image[ImageResize[#,15],ImageSize -> 15]& /@ cfIcons;

cfContentRef /: Normal[cfContentRef[refData_]] := refData; 

cfContentRef[refData_][prop_String] := refData[prop];


(* ::Subsection::Closed:: *)
(*Icon panel*)


cfContentRef /: MakeBoxes[ref:cfContentRef[refData_Association], fmt_] :=
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
		pageData[take -len ,auth, cfHttpGet[auth,data["_links","next"]]],
		{}
	]
] /; MatchQ[data["_links","next"],_String]
pageData[take_,auth_String,data_Association]:= data["results"] /;
	MatchQ[data["_links","next"],_Missing];



(* ::Subsection::Closed:: *)
(*HTTP operations*)


cfHttpGetPaged[auth_ ,args___] := cfHttpGetPaged[1000, auth ,args];
cfHttpGetPaged[take_Integer, auth_ ,args___] := cfHttpGet[auth, args] // (pageData[take,auth, #] /. l_List :> Dataset@l) & 


cfHttpGet[auth_String, path_String] := cfHttpGet[auth, {apiBase<>path}];
cfHttpGet[auth_String, {url_String}] := Module[{req,res},
	req=HTTPRequest[url, <|"Headers"->{
		"Authorization" -> auth
	}|>];
	Sow[req];
	res=URLRead@req;
	Sow[res];
	res /.(r: HTTPResponse[_,KeyValuePattern["StatusCode"->200],___]:>
		(ImportByteArray[r["BodyByteArray"],"RawJSON"] // Dataset))
];


cfHttpPost[auth_String, path_String, body_String] := cfHttpPost[auth, {apiBase<>path}, body];
cfHttpPost[auth_String, {url_String}, body_String] := Module[{req,res},
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


cfHttpPut[auth_String, path_String, body_String] := cfHttpPut[auth, {apiBase<>path}, body];
cfHttpPut[auth_String, {url_String}, body_String] := Module[{req,res},
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


(* ::Section::Closed:: *)
(*Content*)


(* ::Subsection::Closed:: *)
(*Utilities*)


addContentRef[row_Association] := <|
		"cfRef" -> cfContentRef[<|
			"refType" -> refType[row["type"]],
			"contentId" -> row["id"],
			"title" -> row["title"],
			"spaceKey" -> row["space","key"],
			"uiUrl" ->  row["_links","webui"]
		|>],row|>;


(* ::Subsection::Closed:: *)
(*Content Templates*)


pageUpdateTemplate = TemplateObject[<|
	"type" -> "page",
	"title" -> TemplateSlot["title"],
	"body" ->  <|
		"storage"-><|
			"value"-> TemplateSlot["pageHtml"],
			"representation"->"storage"
		|>
	|>
|>];


(* ::Subsection::Closed:: *)
(*uiOpen*)


cfOpenUi[ref_cfContentRef] := SystemOpen[uiBase <> ref["uiUrl"]];


(* ::Subsection::Closed:: *)
(*Info*)


cfInfo[auth_, ref_cfContentRef] := cfGetContent[auth, ref["contentId"]];
cfGetContent[auth_, id_String] := 
	cfHttpGet[auth,StringTemplate["/rest/api/content/``/?``"][id,expandAll<>URLEncode[",body.storage"]]] /.
		ds_Dataset :> ds[addContentRef];


(* ::Subsection::Closed:: *)
(*Search*)


cfContentSearchCount[auth_, query_String] := 
	cfHttpGet[auth,StringTemplate["/rest/api/content/search?cql=``&``"][URLEncode@query,expandAll]] /.
		ds_Dataset :> ds["totalSize"];
cfContentSearch[auth_, args___] := 
	cfContentSearchList[auth, args] /. ds_Dataset :> Normal@ds[All, "cfRef"];
cfContentSearchList[auth_, query_String] := 
	cfContentSearchList[auth, query, 500];
cfContentSearchList[auth_, query_String, take_Integer] := 
	cfHttpGetPaged[take, auth,StringTemplate["/rest/api/content/search?cql=``&``"][URLEncode@query,expandAll]] /.
	ds_Dataset :> ds[All, addContentRef];


(* ::Subsection::Closed:: *)
(*CreatePage*)


cfCreatePage[auth_, parentPage_cfContentRef, data_Association] := 
	cfCreatePage[auth, Append[data, "parentId" -> parentPage["contentId"]]];
cfCreatePage[auth_, KeyValuePattern[{
	"title"-> title_String,
	"parentId" -> parentId_,
	"spaceKey" -> spaceKey_String,
	"pageHtml" -> pageHtml_String	
}]] := Module[{body},
	body = <|
		"type" -> "page",
		"title" -> title,
		"ancestors" -> {<|"id" -> parentId|>}, 
		"space" -> <|"key" -> spaceKey|>,
		"body" ->  <|
			"storage"-><|
				"value"-> pageHtml,
				"representation"->"storage"
			|>
		|>
	|>;
	cfHttpPost[
		auth,
		"/rest/api/content",
		ExportString[body,"RawJSON"]
	] /. ds_Dataset :> ds[addContentRef]
];


(* ::Subsection::Closed:: *)
(*LastVersion*)


cfLastVersion[auth_, ref_cfContentRef] :=
	cfInfo[auth, ref] /. ds_Dataset :> ds["version","number"];


(* ::Subsection::Closed:: *)
(*UpdatePageBody*)


cfUpdatePageBody[auth_, ref_cfContentRef, pageHtml_String] := Module[{body, v, title},
	v = cfLastVersion[auth, ref] + 1;
	title = cfInfo[auth, ref]["title"];
	body = <|
		"title" -> title,
		"type" -> "page",
		"version" -> <|"number" -> v |>,
		"body" ->  <|
			"storage"-><|
				"value"-> pageHtml,
				"representation"->"storage"
			|>
		|>
	|>;
	cfHttpPut[
		auth,
		"/rest/api/content/" <> ref["contentId"],
		ExportString[body,"RawJSON"]
	] /. ds_Dataset :> ds[addContentRef]
];


(* ::Subsection::Closed:: *)
(*Body*)


cfGetBody[ds_Dataset] := cfGetBody@Normal@ds;
cfGetBody[KeyValuePattern["body" -> KeyValuePattern["storage"->KeyValuePattern["value"-> val_String]]]] :=
	val;


cfSetBody[ds_Dataset, value_] := cfSetBody[Normal@ds, value] /. a_Association :> Dataset@a;
cfSetBody[row_Association, value_String] := Module[{r},
	r = row;
	r["body","storage","value"] = value;
	r
];


(* ::Section:: *)
(*Containers*)


(* ::Section::Closed:: *)
(*Footer*)


End[];


EndPackage[]
