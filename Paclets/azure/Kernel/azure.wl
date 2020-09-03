(* ::Package:: *)

(* ::Title:: *)
(*Azure*)


(* ::Section:: *)
(*Notes*)


(* ::Text:: *)
(*To do:*)
(*- rename 	list to resource*)


(* ::Section:: *)
(*Header*)


BeginPackage["azure`"];


(* Base *)
$azExe;
azShellLogin;
azHttpGet;
azHttpGetPaged;
azHttpPost;
azHttpPut;
azHttpPatch;
azHttpDelete;
azShellGetToken;
azRef;
azCreateAzRef;
azParseRefFromId;
azInfo;
azDelete;
azUpdate;
azOpenUi;
azRefToId;
azOperations;
azRestDocumentation;
azIcon;
azRelations;
azRelationPlot;
azDownload;
azFileNames;

azSetUiPortalPrefix;
azGetUiPortalPrefix;

azShellGetSubscriptions;
azShellGetSubscriptionList;

(* Auzure kubernetes service *)
azAksClusters;
azAksClusterList;

(* Log analytics *)
azLogAnalyticsTableHelp;
azLogAnalyticsTableStatistics;
azLogAnalyticsQuery;
azLogAnalyticsWorkspaceMetadata;
azLogAnalyticsWorkspaces;
azLogAnalyticsWorkspaceList;
azLogAnalyticsWorkspaceSearch;
(* Log analytics - kubernetes*)
azLogAnalyticsKubeContainerShortNames;
azLogAnalyticsKubeContainerIdToShortName;
azLogAnalyticsKubeContainerIds;
azLogAnalyticsKubeContainerLogStatistics;
azLogAnalyticsKubeContainerLog;
azLogAnalyticsKubeSearchContainerLogs;

(* API manager *)
azApiManagementServices;
azApiManagementServiceList;
azApiManagementServiceSearch;
azApiManagementLoggers;
azApiManagementLoggerList;
azApiManagementLoggerSearch;
azApiManagementSubscriptions;
azApiManagementSubscriptionList;
azApiManagementSubscriptionSearch;
azApiManagementProducts;
azApiManagementProductList;
azApiManagementProductSearch;
azApiManagementApis;
azApiManagementApiList;
azApiManagementApiSearch;
azApiManagementApiSchemas;
azApiManagementApiSchemaList;
azApiManagementApiSchemaSearch;

(* Event Hubs *)
azEventHubs;
azEventHubNamespaces;
azEventHubNamespaceList;
azEventHubNamespaceSearch;

(* Monitor: activity log *)
azActivityLog;

(* Application Insight *)
azAppInsightComponents;
azAppInsightComponentList;
azAppInsightComponentSearch;

(* Azure DevOps *)
azDevOpsProjects;
azDevOpsProjectList;
azDevOpsProjectSearch;

azDevOpsGitRepositories;
azDevOpsGitRepositoryList;
azDevOpsGitRepositorySearch;
azDevOpsCreateGitRef;
azDevOpsGitRefs;
azDevOpsGitRefList;
azDevOpsGitRefSearch;
azDevOpsGitCommits;
azDevOpsGitCommitList;
azDevOpsGitCommitSearch:
azDevOpsGitCommitGraph;
azDevOpsGitCommitPlot;

azDevOpsBuildDefinitions;
azDevOpsBuildDefinitionCreate;
azDevOpsBuildDefinitionList;
azDevOpsBuildDefinitionSearch;

azDevOpsBuildRuns;
azDevOpsBuildRunList;
azDevOpsBuildRunSearch;
azDevOpsBuildArtifacts;
azDevOpsBuildArtifactList;

azDevOpsReleaseDefinitions;
azDevOpsReleaseDefinitionCreate;
azDevOpsReleaseDefinitionList;
azDevOpsReleaseDefinitionSearch;

azDevOpsReleaseRuns;
azDevOpsReleaseRunCreate;
azDevOpsReleaseRunList;
azDevOpsReleaseRunSearch;
azDevOpsReleaseRunStages;
azDevOpsReleaseRunStageDeploy;

azDevOpsUsers;
azDevOpsUserList;
azDevOpsUserSearch;

azDevOpsGroups;
azDevOpsGroupList;
azDevOpsGroupSearch;

azDevOpsArtifactFeeds;
azDevOpsArtifactFeedList;
azDevOpsArtifactFeedSearch;

azDevOpsArtifactPackages;
azDevOpsArtifactPackageList;
azDevOpsArtifactPackageSearch;

azDevOpsArtifactPackageVersions;
azDevOpsArtifactPackageVersionList;
azDevOpsAritfactPackageVersionSearch;


(* temp *)
azRefDevOpsPattern;
azRefAzurePattern;
getIdKeyValue;
keyValuesFromId;
refLabel;
stdDevOpsResource;
azOpenDevOpsUi;
panelInfo;
devOpsListBuilder;
keyValueContainsQ;
panelInfo;
subscriptionIdFromId;
resourceGroupNameFromId;
azureRelationBuilder;
devOpsSearchBuilder;


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Base*)


(* ::Subsection::Closed:: *)
(*Core*)


relations={};
azRelations[]:=relations;


azRef/:Normal[azRef[a_Association]] := a;
azRef[a_Association][property_String] := a[property];

azCreateAzRef[base_azRef, azType_String,data_Association] := Module[
	{d=Normal@base},
	d["azType"] = azType;
	azRef[<|d,data|>]
]


azSetUiPortalPrefix[prefix_String] := LocalSymbol["azUiPrefix"] = prefix;
azGetUiPortalPrefix[] := LocalSymbol["azUiPrefix"];


azShellLogin[] := RunProcess[{$azExe,"login"}] /. KeyValuePattern["ExitCode"->0] -> Null;

$azExe = "C:\\Program Files (x86)\\Microsoft SDKs\\Azure\\CLI2\\wbin\\az.cmd";

azParseRestResponde[res_] := 
	res /. r:HTTPResponse[_,KeyValuePattern[{"StatusCode"->200}],___] :> Dataset@ImportByteArray[r["BodyByteArray"],"RawJSON"]


azShellGetToken[azRef[KeyValuePattern["subscriptionId" -> id_String]]] := azShellGetToken[id];
azShellGetToken[subscriptionId_String] := RunProcess[{$azExe, "account", "get-access-token"}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_,"--subscription" ->subscriptionId }] :> 
		ImportString[std,"RawJSON"] /. KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
			With[
				{token = ImportByteArray[StringToByteArray@std,"RawJSON"]}, 
				Append[token, "authorizationHeader" -> "Bearer "<> token["accessToken"]]
			];

toIso8601[{from_,to_}] := toIso8601@from<>"/"<>toIso8601@to;
toIso8601[date_DateObject] := DateString[DateObject[date,TimeZone->"Zulu"],"ISODateTime"]<>"Z";

azParseRefFromId[id_String] := StringCases[ToLowerCase@id,
	"/subscriptions/" ~~ subscription:(Except["/"]..) ~~
	"/resourcegroups/"~~resourcegroups:(Except["/"]..) :> 
	<| "subscriptionId" -> subscription , "resourceGroupName" -> resourcegroups |>
] /. {v_} :> v;

azHttpGetPaged[auth_ ,args___] := azHttpGet[auth, args] // (pageData[auth, #] /. l_List :> Dataset@l) & 

Clear[azHttpGet];
azHttpGet[auth_,{urlTemplate_String,azRef[data_Association]}, args___]:=
	azHttpGet[auth,StringTemplate[urlTemplate][URLEncode/@ data],args];
azHttpGet[authorizationHeader_String,  url_String] := Module[
	{req, res},
	req = HTTPRequest[url, <|
		Method -> "GET",
		"ContentType" -> "application/json",
		"Headers" -> {"Authorization" -> authorizationHeader}
	|>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde@res
]

azHttpDelete[authorizationHeader_String,  url_String] := Module[
	{req, res},
	req = HTTPRequest[url, <|
		Method -> "DELETE",
		"ContentType" -> "application/json",
		"Headers" -> {"Authorization" -> authorizationHeader}
	|>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde@res
]

azHttpPost[authorizationHeader_String,  url_String, data_Association] := Module[
	{req, res},
	req = HTTPRequest[url, <|
		Method -> "POST",
		"ContentType" -> "application/json",
		"Headers" -> {"Authorization" -> authorizationHeader},
		"Body" -> ExportString[data,"RawJSON"]
	|>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde@res
]

azHttpPut[authorizationHeader_String,  url_String, data_Association] := Module[
	{req, res},
	req = HTTPRequest[url, <|
		Method -> "PUT",
		"ContentType" -> "application/json",
		"Headers" -> {"Authorization" -> authorizationHeader},
		"Body" -> ExportString[data,"RawJSON"]
	|>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde@res
]

azHttpPatch[authorizationHeader_String,  url_String, data_Association] := Module[
	{req, res},
	req = HTTPRequest[url, <|
		Method -> "PATCH",
		"ContentType" -> "application/json",
		"Headers" -> {"Authorization" -> authorizationHeader},
		"Body" -> ExportString[data,"RawJSON"]
	|>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde@res
]

azDownload[authorizationHeader_String,  url_String, file_String] := Module[
	{res, req},
	req = HTTPRequest[url, <|
		Method -> "GET",
		"ContentType" -> "application/zip",
		"Headers" -> {"Authorization" -> authorizationHeader}
	|>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	BinaryWrite[file,res["BodyByteArray"]];
	Close[file]
]

azFileNames[authorizationHeader_String,  url_String] := Module[
	{res, req},
	req = HTTPRequest[url, <|
		Method -> "GET",
		"ContentType" -> "application/zip",
		"Headers" -> {"Authorization" -> authorizationHeader}
	|>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	ImportByteArray[res["BodyByteArray"],"ZIP"]
]


getIdKeyValue[id_String, idKey_String] := StringCases[
	id,
	idKey~~keyval:(Except["/"]..):>(keyval /. {{}:>Nothing, v_:> v }),
	IgnoreCase->True
]/. {v_} :> v

keyValuesFromId[id_String, idTemplate_String] :=
	getIdKeyValue[id, #[[1]],#[[2]]]& /@ 
		(StringTemplate[idTemplate] /. 
			TemplateObject[l_List,___] :> l //
			 SequenceCases[#,{p_,TemplateSlot[k_]}:> {p,k}]&
		) /. l_List :> Association@l; 
		
refValues[ref:azRef[refData_Association]] := 
	Append[URLEncode /@ refData, "id" -> azRefToId[ref]];
		
pageData[auth_String, r_HTTPResponse] := r;
pageData[auth_String, ds_Dataset] := pageData[auth, Normal@ds];
pageData[auth_String,data_Association] := (
	PrintTemporary[StringTemplate["Paging: ``"][Length@data["value"]]];
	data["value"] ~ Join ~ pageData[auth, azHttpGet[auth,data["nextLink"]]]) /;
				KeyExistsQ[data,"nextLink"];
pageData[auth_String,data_Association]:=data["value"] /;
				!KeyExistsQ[data,"nextLink"];


azRefAzurePattern[azType_String] := azRefAzurePattern[azType, <||>];
azRefAzurePattern[azType_String,asc_Association] := With[
	{inner = <|
		"azType"->azType,
		"subscriptionId"-> subscriptionId_String,
		asc
	|>},
	ref:azRef[refData:KeyValuePattern@Normal@inner]
]


azOpenDevOpsUi[url_String] := Module[{},
	Sow[url];
	SystemOpen[url];
];

azOpenUi[path_String] := Module[{url},
	url = StringTemplate["https://portal.azure.com/``.onmicrosoft.com``"][azGetUiPortalPrefix[],path];
	Sow[url];
	SystemOpen[url];
];
azOpenUi[KeyValuePattern["azRef" -> ref_]] := azOpenUi@ref;
azOpenUi[ds_Dataset] := azOpenUi@Normal@ds;


azRelationPlot[]:=Module[{rel=azRelations[],edges,vertices},
edges=(#/.{t1_->t2_,l_}:>Tooltip[(t1->t2),TableForm@l])&/@rel;
vertices=(#/.{t1_->t2_,_}:>Splice[{t1->Tooltip[azIcon[t1],t1],t2->Tooltip[azIcon[t2],t2]}])&/@rel;
GraphPlot[edges,DirectedEdges->True,VertexShape->vertices,VertexSize->0.2,GraphLayout->"RadialEmbedding"]
]


(* ::Subsection::Closed:: *)
(*Azure*)


subscriptionIdFromId[id_String] := 
	StringCases[id,StartOfString~~"/subscriptions/"~~sub:(Except["/"]..) :> sub, IgnoreCase->True] /. {v_} :> v;
resourceGroupNameFromId[id_String] := 
	StringCases[id,StartOfString~~"/subscriptions/"~~(Except["/"]..)~~"/resourcegroups/"~~rg:(Except["/"]..) :> rg, IgnoreCase->True] /. {v_} :> v;


azurePanelInfoBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"panelIcon" ->_Image,
	"panelLabelFunc"->_
}]] := TemplateObject[Hold[

panelInfo[TemplateSlot["azType"]] := <|
	"icon" -> TemplateSlot["panelIcon"],
	"labelFunc" -> TemplateSlot["panelLabelFunc"]
|>;
azIcon[azRefAzurePattern[TemplateSlot["azType"]]] := azIcon[refData["azType"]];
azIcon[TemplateSlot["azType"]] := TemplateSlot["panelIcon"];

]][cfg] // ReleaseHold

azureOpenUiBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"uiUrl"-> _String
}]] := TemplateObject[Hold[
azOpenUi[azRefAzurePattern[TemplateSlot["azType"]]] := Module[{url},
	url=StringTemplate[TemplateSlot["uiUrl"]][URLEncode /@ refData];
	azOpenUi[url];
]]][cfg] // ReleaseHold

azureInfoBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"getUrl"->_String
}]] := TemplateObject[Hold[
azInfo[authorizationHeader_String, azRefAzurePattern[TemplateSlot["azType"]]] := 
	azHttpGet[authorizationHeader,
		StringTemplate[TemplateSlot["getUrl"]][URLEncode/@refData]
	] /. ds_Dataset :> ds[<|
		"azRef" -> azRef[<|
				"azType" -> TemplateSlot["azType"],
				TemplateSlot["listResultKeysFunc"][#]
			|>],
		 # 
	|> &]
]][cfg] // ReleaseHold

azureListBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"nameSingular" -> _String,
	"namePlural" -> _String,
	"listFilter" -> _,
	"listUrl" -> _String,
	"listResultKeysFunc" -> _
}]] := TemplateObject[Hold[
Symbol["az"<>TemplateSlot["namePlural"]][args___] := Symbol["az"<>TemplateSlot["nameSingular"]<>"List"][args] /. ds_Dataset :> Normal@ds[All,"azRef"];
Symbol["az"<>TemplateSlot["nameSingular"]<>"List"][authorizationHeader_String, TemplateSlot["listFilter"]] := 
	azHttpGet[authorizationHeader, 
		StringTemplate[TemplateSlot["listUrl"]][URLEncode /@ refData]] /.
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> TemplateSlot["azType"],
				TemplateSlot["listResultKeysFunc"][#]
			|>], #|> &];
]][cfg] // ReleaseHold

azureRelationBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"parentAzType" -> _String,
	"namePlural" -> _String
}]] := TemplateObject[Hold[
AppendTo[relations, {TemplateSlot["parentAzType"]->TemplateSlot["azType"], {"az"<>TemplateSlot["namePlural"],"az"<>TemplateSlot["nameSingular"]<>"List"}}]
]][cfg] // ReleaseHold;

azureRestDocumentationBuilder[cfg:KeyValuePattern[{
	"azType"->_String
}]] := TemplateObject[Hold[
azRestDocumentation[azRefAzurePattern[TemplateSlot["azType"]]] :=
	SystemOpen[TemplateSlot["restDocumentation"]]
]][cfg] // ReleaseHold;

azureSearchBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"nameSingular" -> _String,
	"listFilter" -> _,
	"listUrl" -> _,
	"searchFields" -> {_String..}
}]] := TemplateObject[Hold[
Symbol["az"<>TemplateSlot["nameSingular"]<>"Search"][auth_String, TemplateSlot["listFilter"], pattern_] := Module[{query},
	query = Select[keyValueContainsQ[#, TemplateSlot["searchFields"], pattern] &];
	Sow[query];
	Symbol["az"<>TemplateSlot["nameSingular"]<>"List"][auth, ref][
		query
	]]
]][cfg] // ReleaseHold

azureDefaultOperationsBuilder[cfg_Association] := (
	azurePanelInfoBuilder[cfg];
	If[KeyExistsQ[cfg, "uiUrl"],
		azureOpenUiBuilder[cfg],
		Null];
	azureInfoBuilder[cfg];
	azureListBuilder[cfg];
	azureRestDocumentationBuilder[cfg];
	If[KeyExistsQ[cfg, "searchFields"],
		azureSearchBuilder[cfg],
		Null];
	If[KeyExistsQ[cfg, "parentAzType"],
		azureRelationBuilder[cfg],
		Null];
);



(* ::Subsection:: *)
(*DevOps*)


devOpsPanelInfoBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"panelIcon" ->_Image,
	"panelLabelFunc"->_
}]] := TemplateObject[Hold[
panelInfo[TemplateSlot["azType"]] := <|
	"icon" -> TemplateSlot["panelIcon"],
	"labelFunc" -> TemplateSlot["panelLabelFunc"]
|>;
azIcon[azRefAzurePattern[TemplateSlot["azType"]]] := azIcon[refData["azType"]];
azIcon[TemplateSlot["azType"]] := TemplateSlot["panelIcon"];
]][cfg] // ReleaseHold

devOpsOpenUiBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"uiUrl"-> _
}]] := TemplateObject[Hold[
azOpenUi[azRefDevOpsPattern[TemplateSlot["azType"]]] := 
	azOpenDevOpsUi[StringTemplate[TemplateSlot["uiUrl"]][URLEncode /@ refData]]
]][cfg] // ReleaseHold

devOpsInfoBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"getUrl"->_String
}]] := TemplateObject[Hold[
azInfo[authorizationHeader_String, azRefDevOpsPattern[TemplateSlot["azType"]]] := Module[
	{res},
	res = azHttpGet[authorizationHeader,
		StringTemplate[TemplateSlot["getUrl"]][URLEncode/@refData]
	];
	res = res /. ds_Dataset :> Dataset[Normal[res]/. KeyValuePattern[{"value" -> v_, "count"->1}] :> First@v];
	res /. ds_Dataset :> ds[<|
		"azRef" -> azRef[<|
				"azType" -> TemplateSlot["azType"],
				TemplateSlot["listResultKeysFunc"][#]
			|>],
		 # 
	|> &]
]
]][cfg] // ReleaseHold

devOpsListBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"nameSingular" -> _String,
	"namePlural" -> _String,
	"listFilter" -> _,
	"listUrl" -> _String,
	"listResultKeysFunc" -> _
}]] := TemplateObject[Hold[
Symbol["azDevOps"<>TemplateSlot["namePlural"]][args___] := Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"List"][args] /. ds_Dataset :> Normal@ds[All,"azRef"];
Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"List"][authorizationHeader_String, TemplateSlot["listFilter"]] := 
	azHttpGet[authorizationHeader, 
		StringTemplate[TemplateSlot["listUrl"]][URLEncode /@ refData]] /.
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> TemplateSlot["azType"],
				TemplateSlot["listResultKeysFunc"][#]
			|>], #|> &]
]][cfg] // ReleaseHold

devOpsRelationBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"parentAzType" -> _String,
	"namePlural" -> _String
}]] := TemplateObject[Hold[
AppendTo[relations, {TemplateSlot["parentAzType"]->TemplateSlot["azType"], {"azDevOps"<>TemplateSlot["namePlural"],"azDevOps"<>TemplateSlot["nameSingular"]<>"List"}}]
]][cfg] // ReleaseHold;

devOpsRestDocumentationBuilder[cfg:KeyValuePattern[{
	"azType"->_String
}]] := TemplateObject[Hold[
azRestDocumentation[azRefDevOpsPattern[TemplateSlot["azType"]]] :=
	SystemOpen[TemplateSlot["restDocumentation"]]
]][cfg] // ReleaseHold

devOpsSearchBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"nameSingular" -> _String,
	"listFilter" -> _,
	"listUrl" -> _,
	"searchFields" -> {_String..}
}]] := TemplateObject[Hold[
Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"Search"][auth_String, TemplateSlot["listFilter"], pattern_] := Module[{query},
	query = Select[keyValueContainsQ[#, TemplateSlot["searchFields"], pattern] &];
	Sow[query];
	Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"List"][auth, ref][
		query
	]]
]][cfg] // ReleaseHold

devOpsCreateBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"nameSingular" -> _String,
	"listUrl" -> _String
}]] := TemplateObject[Hold[
Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"Create"][auth_String, TemplateSlot["listFilter"], data_Dataset] :=
	Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"Create"][auth, ref, Normal@data];
Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"Create"][auth_String, TemplateSlot["listFilter"], data_Association] :=
	azHttpPost[auth, StringTemplate[TemplateSlot["listUrl"]][URLEncode /@ refData], KeyDrop[data,"azRef"]] /. ds_Dataset :> ds[<|
		"azRef" -> azRef[<|
				"azType" -> TemplateSlot["azType"],
				TemplateSlot["listResultKeysFunc"][#]
			|>],
		 # 
	|> &]	
]][cfg] // ReleaseHold

devOpsUpdateBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"getUrl"->_String
}]] := TemplateObject[Hold[
azUpdate[auth_String, data_Dataset] :=
	azUpdate[auth, data["azRef"] ,data];
azUpdate[auth_String, azRefDevOpsPattern[TemplateSlot["azType"]], data_Dataset] :=
	azUpdate[auth, ref, KeyDrop[Normal@data, "azRef"]];
azUpdate[authorizationHeader_String, azRefDevOpsPattern[TemplateSlot["azType"]], data_Association] := 
	azHttpPut[authorizationHeader,
		StringTemplate[TemplateSlot["getUrl"]][URLEncode/@refData],
		KeyDrop[data,"azRef"]
	] /. ds_Dataset :> ds[<|
		"azRef" -> azRef[<|
				"azType" -> TemplateSlot["azType"],
				TemplateSlot["listResultKeysFunc"][#]
			|>],
		 # 
	|> &]
]][cfg] // ReleaseHold

devOpsDeleteBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"getUrl"->_String
}]] := TemplateObject[Hold[
azDelete[authorizationHeader_String, azRefDevOpsPattern[TemplateSlot["azType"]]] :=
	azHttpDelete[authorizationHeader,
		StringTemplate[TemplateSlot["getUrl"]][URLEncode/@refData]
	] 
]][cfg] // ReleaseHold


devOpsDefaultOperationsBuilder[cfg_Association] := (
	devOpsPanelInfoBuilder[cfg];
	If[KeyExistsQ[cfg,"uiUrl"],
		devOpsOpenUiBuilder[cfg],
		Null];
	devOpsInfoBuilder[cfg];
	devOpsListBuilder[cfg];
	devOpsRestDocumentationBuilder[cfg];
	If[KeyExistsQ[cfg, "searchFields"],
		devOpsSearchBuilder[cfg],
		Null];
	devOpsCreateBuilder[cfg];
	devOpsUpdateBuilder[cfg];
	devOpsDeleteBuilder[cfg];
	If[KeyExistsQ[cfg, "parentAzType"],
		devOpsRelationBuilder[cfg],
		Null];	
);


(* ::Section::Closed:: *)
(*azRef panel*)


(* ::Input:: *)
(**)


(* Notebook define azIcons variable *)
NotebookImport[DirectoryName@FindFile@"azure`"<>"icons.nb","Input"->"Expression"];
icons =  Image[#,ImageSize->15]& /@ azIcons;
panelInfo[_] := <|"icon"->icons["azure.default"],"labelFunc"->("???"&)|>

refType[KeyValuePattern["azType"->type_String]] := (Last@StringSplit[type,"."] // (Last@StringSplit[#,"/"] &)) /; 
	StringContainsQ[type,"."];
refType[_] := "unkown type";

azRef /: MakeBoxes[ref:azRef[refData_Association], fmt_] :=
Block[{type, bg, display, panelInf},
	panelInf = panelInfo[refData["azType"]];
	bg = Which[
		StringContainsQ[refData["azType"],"azure."], Lighter[LightBlue],
		StringContainsQ[refData["azType"],"devOps."], Lighter[LightGreen],
		True, Lighter[Lighter[LightRed]]
	];
	type =  refType[refData];
	display = Tooltip[
		Framed[
			Row[{panelInf["icon"]," ",Style[type, Italic], ": ", Style[panelInf["labelFunc"][refData],Bold]}],
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
]



(* ::Section::Closed:: *)
(*Subscriptions*)


cfg = <|
	"azType"->"azure.subscription",
	"nameSingular"->"Subscription",
	"namePlural"->"Subscriptions",
	"panelIcon"-> icons["azure.subscription"],
	"panelLabelFunc"-> Function[{refData}, refData["subscriptionName"]],
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/overview"
|>;
azurePanelInfoBuilder[cfg];

azShellGetSubscriptions[args___] := azShellGetSubscriptionList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azShellGetSubscriptionList[] := RunProcess[{$azExe ,"account","list"}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
		Dataset[ImportByteArray[StringToByteArray@std,"RawJSON"]][
			All, <| "azRef" -> azRef[<|
				"azType" -> "azure.subscription",
				"subscriptionName" -> #["name"],
				"subscriptionId" -> #["id"]
			|>], # |> &]



(*
refLabel[ref:KeyValuePattern["azType" -> "azure.subscription"]] := ref["subscriptionName"];
refIcon[KeyValuePattern["azType" -> "azure.subscription"]] := icons["azure.subscription"];

azOpenUi[azRefAzurePattern["azure.subscription"]] := Module[{url},
	url = StringTemplate["https://portal.azure.com/#@santander.onmicrosoft.com/resource/subscriptions/`subscriptionId`/overview"]
		[URLEncode /@ refData];
	azOpenUi[url];
];

azShellGetSubscriptions[args___] := azShellGetSubscriptionList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azShellGetSubscriptionList[] := RunProcess[{$azExe ,"account","list"}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
		Dataset[ImportByteArray[StringToByteArray@std,"RawJSON"]][
			All, <| "azRef" -> azRef[<|
				"azType" -> "azure.subscription",
				"subscriptionName" -> #["name"],
				"subscriptionId" -> #["id"]
			|>], # |> &]
*)


(* ::Section::Closed:: *)
(*Azure kubernetes service*)


<|
	"name"-> {"Aks","Cluster"},
	"idTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ContainerService/managedClusters/`clusterName`",
	"getUrl" -> "https://management.azure.com`id`?api-version=2020-06-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.ContainerService/managedClusters?api-version=2020-06-01",
	"listFilter" -> "azure.subscription",
	"uiUrl"->"/resource`id`/overview",
	"azType"->"azure.Aks/cluster",
	"azIcon"->"azure.aks",
	"azLabel"->"refData[\"clusterName\"]",
	"restDocumentation" -> "https://docs.microsoft.com/en-us/rest/api/aks/managedclusters"
|> // stdAzureResource //ToExpression


(* ::Section::Closed:: *)
(*Log analytics*)


(* ::Text:: *)
(*https://dev.loganalytics.io/documentation/Overview*)
(*https://docs.microsoft.com/en-us/azure/application-gateway/application-gateway-diagnostics*)
(*https://docs.microsoft.com/en-us/rest/api/loganalytics/dataaccess/metadata/get*)
(*https://docs.microsoft.com/en-us/azure/azure-monitor/log-query/log-query-overview*)
(*https://docs.microsoft.com/en-us/azure/azure-monitor/reference/*)


(* ::Subsection::Closed:: *)
(*Tables*)


azLogAnalyticsTableHelp[table_String] :=
	SystemOpen["https://docs.microsoft.com/en-us/azure/azure-monitor/reference/tables/"<>table];


azLogAnalyticsTableStatistics[authorizationHeader_String,ref_] := azLogAnalyticsTableStatistics[authorizationHeader,ref, {Now-Quantity[24,"Hours"],Now}];
azLogAnalyticsTableStatistics[authorizationHeader_String,ref_, dateRange:{_DateObject,_DateObject}] :=
	azLogAnalyticsQuery[authorizationHeader, ref,"
		Usage
		| where IsBillable == true
	", dateRange] /. ds_Dataset :> ds["Table_0",GroupBy["DataType"],Total /* (UnitConvert[#,Quantity[1,"Gigabytes"]]&),Quantity[#Quantity,#QuantityUnit]&][ReverseSort]


(* ::Subsection::Closed:: *)
(*Workspaces*)


cfg = <|
	"azType"->"azure.logAnalytics.Workspace",
	"nameSingular"->"LogAnalyticsWorkspace",
	"namePlural"->"LogAnalyticsWorkspaces",
	"panelIcon"-> icons["azure.logAnalyticsWorkspace"],
	"panelLabelFunc"-> Function[{refData}, refData["workspaceName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/loganalytics/workspaces",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`/Overview",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`?api-version=2020-03-01-preview",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.OperationalInsights/workspaces?api-version=2020-03-01-preview",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"workspaceName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
(*Meta data*)


azLogAnalyticsWorkspaceMetadata[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"azType" -> "azure.logAnalytics.Workspace"
}]]] :=  azHttpGet[
	authorizationHeader, 
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`/api/metadata?api-version=2017-01-01-preview"][ref]
]



(* ::Subsection::Closed:: *)
(*Query*)


azLogAnalyticsQuery[
	authorizationHeader_String,
	workspace_azRef, 
	query_String
] := azLogAnalyticsQuery[authorizationHeader, workspace, query, {DateObject[{1800}],DateObject[{3000}]}];
azLogAnalyticsQuery[
	authorizationHeader_String,
	workspace_azRef,
	query_String,
	Null
] := azLogAnalyticsQuery[authorizationHeader, workspace, query];
azLogAnalyticsQuery[
	authorizationHeader_String,
	azRef[resource:KeyValuePattern[{
		"azType" -> "azure.logAnalytics.Workspace"
	}]], 
	query_String,
	dateRange:{from_DateObject,to_DateObject}
] := Module[ {url,req,res,body},
	Sow[{query,dateRange}];
	url = StringTemplate[
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`/api/query?api-version=2017-01-01-preview"][resource];
	body = ExportString[<|"query"->query, "timespan" -> toIso8601[dateRange]|>,"RawJSON"];
	req = HTTPRequest[url, <| Method->"POST", "Body"->body, "ContentType"->"application/json", "Headers"->{"Authorization"-> authorizationHeader} |>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde@res /. ds_Dataset :> logAnalyticDS[ds]
]

logAnalyticFieldValue[_,Null] := Null;
logAnalyticFieldValue["SByte",False] := False;
logAnalyticFieldValue["SByte",True] := True;
logAnalyticFieldValue["Guid",v_] := v;
logAnalyticFieldValue["Int64",v_] := v;
logAnalyticFieldValue["Int32",v_] := v;
logAnalyticFieldValue["Double",v_] := v;
logAnalyticFieldValue["DateTime", v_String] := DateObject[v ,TimeZone->"Zulu"];
logAnalyticFieldValue["String", v_String] := v 
logAnalyticField[col_, valueStr_] := col["ColumnName"]->logAnalyticFieldValue[col["DataType"],valueStr];
logAnalyticRow[columns_List,rows_List] := MapThread[logAnalyticField,{columns,rows}] // Association;
logAnalyticTable[columns_List,rows_List] := logAnalyticRow[columns,#]& /@ rows;
logAnalyticDS[ds_Dataset] := #TableName->logAnalyticTable[#Columns,#Rows]&/@ Normal[ds["Tables"]] // Association // Dataset;


(* ::Subsection::Closed:: *)
(*AKS*)


(* ::Text:: *)
(*https://docs.microsoft.com/en-us/azure/azure-monitor/insights/containers*)


azLogAnalyticsKubeContainerShortNames[auth_,ref_] :=
	azLogAnalyticsQuery[auth, ref, "KubePodInventory | distinct ContainerName "] /. ds_Dataset :> 
			Sort@DeleteDuplicates[Last@StringSplit[#,"/"]& /@ (Normal@ ds["Table_0",All,"ContainerName"] /. "" -> Nothing)];


azLogAnalyticsKubeContainerIds[auth_,ref_, containerNameFilter_String] := 
	azLogAnalyticsQuery[auth, ref, StringTemplate["KubePodInventory | where ContainerName has '``' | distinct ContainerID"][containerNameFilter]] /.
		ds_Dataset :> (Normal[ds["Table_0",All,"ContainerID"]] /. "" -> Nothing);
		
azLogAnalyticsKubeContainerIds[auth_,ref_] := 
	azLogAnalyticsQuery[auth, ref, StringTemplate["KubePodInventory  | distinct ContainerID"][containerNameFilter]] /.
		ds_Dataset :> (Normal[ds["Table_0",All,"ContainerID"]] /. "" -> Nothing)


azLogAnalyticsKubeContainerLogStatistics[auth_,ref_ , containerId_] := 
	azLogAnalyticsQuery[auth, ref, StringTemplate[
		"ContainerLog | where ContainerID == '`1`' | summarize ContainerId='`1`',StartLogTime=min(TimeGenerated),EndLogTime=max(TimeGenerated),Count=count()"
	][containerId]] /. 
		ds_Dataset :> Normal[ds["Table_0",SortBy["StartLogTime"]]] /.{v_}:>v


azLogAnalyticsKubeContainerLog[auth_,ref_, containerIds_List, dateRange_] := 
	Dataset[Flatten[Normal@azLogAnalyticsKubeContainerLog[auth,ref,#,dateRange] & /@ containerIds]];

azLogAnalyticsKubeContainerLog[auth_,ref_, containerId_String, dateRange_: Null] := 
	azLogAnalyticsQuery[auth, ref, StringTemplate[
		"ContainerLog | where ContainerID == '``' | project TimeGenerated,LogEntry, ContainerID "][containerId], 
		dateRange] /.
			ds_Dataset :> ds["Table_0", SortBy["TimeGenerated"]];
			
azLogAnalyticsKubeContainerLog[auth_,ref_, All, dateRange_] := 
	azLogAnalyticsQuery[auth, ref, 
		"ContainerLog | project TimeGenerated,LogEntry, ContainerID ", 
		dateRange] /.
			ds_Dataset:>ds["Table_0", SortBy["TimeGenerated"]]


azLogAnalyticsKubeContainerIdToShortName[auth_,ref_, containerId_String] :=
	azLogAnalyticsQuery[auth, ref, 
		StringTemplate["KubePodInventory | where ContainerID == '``' | take 1 "][containerId]] /.
			ds_Dataset :> Last@StringSplit[Normal@ds["Table_0",1,"ContainerName"], "/"]


azLogAnalyticsKubeSearchContainerLogs[auth_,ref_, str_String, dateRange_: Null] := azLogAnalyticsQuery[auth, ref, StringTemplate[
	"ContainerLog | where LogEntry  contains '``' | take 1000 | project TimeGenerated,LogEntry,ContainerID "][str], dateRange] /.
		ds_Dataset:>ds["Table_0"]


(* ::Section::Closed:: *)
(*Monitor*)


azActivityLog[
	authorizationHeader_String,
	ref:azRef[KeyValuePattern["azType" -> _String]],
	dateRange:{_DateObject, _DateObject}
] := Module[{},
	azActivityLog[
		authorizationHeader,
		ref,
		StringTemplate["
			eventTimestamp ge '2014-07-16T04:36:37.6407898Z' 
			and eventTimestamp le '2014-07-20T04:36:37.6407898Z' 
			and resourceUri eq 'resourceURI'
		"][refToIdUrl[ref]]
	]
];


azActivityLog[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern["subscriptionId" -> _String]],
	filter_String
] := azHttpGetPaged[
	authorizationHeader,
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/providers/microsoft.insights/eventtypes/management/values?api-version=2015-04-01&$filter=`filter`"]
		[URLEncode /@ <|ref, "filter" -> filter |>]
];


(* ::Section::Closed:: *)
(*API manager*)


(* ::Subsection::Closed:: *)
(*Services*)


cfg = <|
	"azType"->"azure.apiManagement.service",
	"nameSingular"-> "ApiManagementService",
	"namePlural"-> "ApiManagementServices",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["serviceName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apimanagementservice",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/overview",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.ApiManagement/service?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
(*Loggers*)


cfg = <|
	"azType"->"azure.apiManagement.logger",
	"nameSingular"-> "ApiManagementLogger",
	"namePlural"-> "ApiManagementLoggers",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["loggerName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/logger",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`ServiceName`/appInsights",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/loggers/`loggerName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/loggers?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.service"],
	"parentAzType" -> "azure.apiManagement.service",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"loggerName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;


azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
(*Subscriptions*)


cfg = <|
	"azType"->"azure.apiManagement.subscriptions",
	"nameSingular"-> "ApiManagementSubscription",
	"namePlural"-> "ApiManagementSubscriptions",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["subscriptionName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/subscription",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-subscriptions",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/subscriptions/`subscriptionName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/subscriptions?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.service"],
	"parentAzType" -> "azure.apiManagement.service",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"subscriptionName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;


azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
(*Products*)


cfg = <|
	"azType"->"azure.apiManagement.product",
	"nameSingular"-> "ApiManagementProduct",
	"namePlural"-> "ApiManagementProducts",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["productName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/product",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-products",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/products/`productName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/products?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.service"],
	"parentAzType" -> "azure.apiManagement.service",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"productName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;


azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
(*API's*)


cfg = <|
	"azType"->"azure.apiManagement.api",
	"nameSingular"-> "ApiManagementApi",
	"namePlural"-> "ApiManagementApis",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["apiName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apis",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.service"],
	"parentAzType" -> "azure.apiManagement.service",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;



azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
(*API Schema's*)


cfg = <|
	"azType"->"azure.apiManagement.api.schema",
	"nameSingular"-> "ApiManagementApiSchema",
	"namePlural"-> "ApiManagementApiSchemas",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["schemaName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apischema",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/schemas/`schemaName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/schemas?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.api"],
	"parentAzType" -> "azure.apiManagement.api",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> getIdKeyValue[res["id"],"apis/"],
		"schemaName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg];


(* ::Section::Closed:: *)
(*Event Hubs*)


(* ::Subsubsection::Closed:: *)
(*Namespaces*)


cfg = <|
	"azType"->"azure.eventHub.namespace",
	"nameSingular"-> "EventHubNamespace",
	"namePlural"-> "EventHubNamespaces",
	"panelIcon"-> icons["azure.eventHub"],
	"panelLabelFunc"-> Function[{refData}, refData["namespaceName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/eventhub/event-hubs-runtime-rest",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.EventHub/namespaces",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionName`/resourceGroups/`resourceGroupName`/providers/Microsoft.EventHub/namespaces/`namespaceName`?api-version=2018-01-01-preview",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.EventHub/namespaces?api-version=2018-01-01-preview",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"namespaceName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg];


(* ::Section::Closed:: *)
(*Application Insights*)


(* ::Subsection::Closed:: *)
(*Components*)


cfg = <|
	"azType"->"azure.applicationInsight.component",
	"nameSingular"-> "AppInsightComponent",
	"namePlural"-> "AppInsightComponents",
	"panelIcon"-> icons["azure.applicationInsight"],
	"panelLabelFunc"-> Function[{refData}, refData["componentName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/application-insights/components",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/microsoft.insights/components/`componentName`/overview",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Insights/components/`componentName`?api-version=2015-05-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Insights/components?api-version=2015-05-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"componentName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg];


(* ::Section:: *)
(*DevOps*)


(* ::Text:: *)
(*https://docs.microsoft.com/en-us/rest/api/azure/devops/?view=azure-devops-rest-6.0*)


(* ::Subsection::Closed:: *)
(*Common*)


azRefDevOpsPattern[azType_String] := azRefDevOpsPattern[azType, <||>];
azRefDevOpsPattern[azType_String,asc_Association] := With[
	{inner = <|
		"azType"->azType,
		asc
	|>},
	ref:azRef[refData:KeyValuePattern@Normal@inner]
]

devOpsProjectIdFromUrl[url_String] := 
	StringCases[url,projId:(Except["/"]..)~~"/_" :> projId] /. {v_}:>v
	
devOpsOrgFromUrl[url_String] :=
	StringCases[url,("//"~~(Except["/"]..)~~"/"~~org:(Except["/"]..)):>org]/. {v_}:>v
	
keyValueContainsQ[data_Association, keys_List, pattern_] := 
	Or@@(keyValueContainsQ[data,#,pattern] &/@ keys);
keyValueContainsQ[data_Association, key_String, pattern_] := 
	StringContainsQ[data[key], pattern, IgnoreCase->True] /; KeyExistsQ[data,key];
keyValueContainsQ[data_Association, key_String, pattern_] := False;	


(* ::Subsection::Closed:: *)
(*Organization*)


panelInfo["devOps.organization"] := <|
	"icon"->icons["devOps.organization"],
	"labelFunc"->Function[{refData},refData["organizationName"]]
|>;

azIcon[azRefAzurePattern["devOps.organization"]] := azIcon[refData["azType"]];
azIcon["devOps.organization"] := icons["devOps.organization"];


(* ::Subsection::Closed:: *)
(*Projects*)


<|
	"azType"->"devOps.project",
	"nameSingular"->"Project",
	"namePlural"->"Projects",
	"panelIcon"-> icons["devOps.default"],
	"panelLabelFunc"-> Function[{refData},refData["projectName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/?view=azure-devops-rest-6.0", 
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectName`",
	"getUrl"->"https://dev.azure.com/`organizationName`/_apis/projects/`projectName`?api-version=6.0-preview.4",
	"listUrl" -> "https://dev.azure.com/`organizationName`/_apis/projects?api-version=2.0",
	"listFilter" -> azRefDevOpsPattern["devOps.organization"],
	"parentAzType" -> "devOps.organization",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> res["id"],
		"projectName" -> res["name"] 
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder



(* ::Subsection::Closed:: *)
(*Git*)


(* ::Subsubsection::Closed:: *)
(*Repositories*)


<|
	"azType"->"devOps.git.repository",
	"nameSingular"->"GitRepository",
	"namePlural"->"GitRepositories",
	"panelIcon"-> icons["devOps.repository"],
	"panelLabelFunc"-> Function[{refData},refData["repositoryName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/git/repositories?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_git/`repositoryName`",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`?api-version=6.0-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"repositoryName" -> res["name"],
		"repositoryId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder



(* ::Subsubsection::Closed:: *)
(*Refs*)


<|
	"azType"->"devOps.git.ref",
	"nameSingular"->"GitRef",
	"namePlural"->"GitRefs",
	"panelIcon"-> icons["devOps.repository"],
	"panelLabelFunc"-> Function[{refData},refData["refName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/git/refs?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_git/APIs?version=GB<* Last@StringSplit[#refName,URLEncode[\"/\"]] *>",
	"getUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`/refs?filter=`refName`&includeLinks=True&latestStatusesOnly=True",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`/refs?api-version=6.0-preview.1&includeLinks=True&latestStatusesOnly=True",
	"listFilter" -> azRefDevOpsPattern["devOps.git.repository"],
	"parentAzType" -> "devOps.git.repository",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"repositoryId" -> getIdKeyValue[res["url"],"repositories/"],
		"refName" -> StringReplace[res["name"],"refs/"->""]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder 

azDevOpsGitRefList[authorizationHeader_String, azRefDevOpsPattern["devOps.git.commit"]] :=Module[{repo,commit},
	commit = ref;
	repo = ReplacePart[ref,{1,"azType"}->"devOps.git.repository"];
	If[MemberQ[azDevOpsGitCommits[authorizationHeader,#],commit],#,Nothing] & /@ 
		azDevOpsGitRefs[authorizationHeader,repo]
];

AppendTo[relations, {"devOps.git.ref"->"devOps.git.commit", {"azDevOpsGitRefs","azDevOpsGitRefList"}}];


(* ::Subsubsection::Closed:: *)
(*Commits*)


(conf = <|
	"azType"->"devOps.git.commit",
	"nameSingular"->"GitCommit",
	"namePlural"->"GitCommits",
	"panelIcon"-> icons["devOps.repository"],
	"panelLabelFunc"-> Function[{refData},StringTake[refData["commitId"],6]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/git/commits?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_git/APIs/commit/`commitId`",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`/commits/`commitId`?api-version=6.0-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`/commits?api-version=6.0-preview.1&searchCriteria.includeLinks=true&$top=1000&searchCriteria.includePushData=true",
	"listFilter" -> azRefDevOpsPattern["devOps.git.repository"],
	"parentAzType" -> "devOps.git.repository",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"repositoryId" -> getIdKeyValue[res["url"],"repositories/"],
		"commitId" -> res["commitId"]
	|>],
	"searchFields" -> {"commitId"}
|>) // devOpsDefaultOperationsBuilder

azDevOpsGitCommitList[authorizationHeader_String, azRefDevOpsPattern["devOps.git.ref"]] := 
	azHttpGet[authorizationHeader, 
		StringTemplate["https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`/commits?searchCriteria.itemVersion.version=<* Last@StringSplit[#refName,URLEncode[\"/\"]] *>&api-version=6.0-preview.1&$top=1000"][URLEncode /@ refData]] /.
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.git.commit",
				Function[{res}, <| 
					"organizationName" -> devOpsOrgFromUrl[res["url"]], 
					"projectId" -> devOpsProjectIdFromUrl[res["url"]],
					"repositoryId" -> getIdKeyValue[res["url"],"repositories/"],
					"commitId" -> res["commitId"] |>][#]
			|>], #|> &]

AppendTo[relations, {"devOps.git.commit"->"devOps.git.ref", {"azDevOpsGitCommits","azDevOpsGitCommitList"}}];

azInfo[auth_, commits:{azRef[KeyValuePattern["azType" -> "devOps.git.commit"]]..}] :=
	Dataset@Flatten[Normal@azInfo[auth,#] & /@ commits];


azDevOpsGitCommitGraph[commitDS_Dataset]:=Module[{vertices,edges},
	vertices = commitDS[All,Annotation[#commitId,"data"->#]&] // Normal;
	edges = Flatten@Normal@commitDS[All,Function[{row},{row["commitId"]->#& /@ row["parents"]}]];
	Graph[vertices,edges]
]


azDevOpsGitCommitPlot[g_Graph]:=GraphPlot[
	g,
	GraphLayout->"LayeredDigraphEmbedding",
	VertexShapeFunction->
	Function[{xy,name,wh},
		With[{data=AnnotationValue[ {g,name},"data"]},
			Tooltip[
				EventHandler[Disk[xy,First@wh],{"MouseClicked":>CreateDialog@Dataset@data}],
				data["comment"]
			]
		]
	],
	DirectedEdges->True
];


(* ::Subsection::Closed:: *)
(*Build*)


(* ::Subsubsection::Closed:: *)
(*Definition*)


<|
	"azType"->"devOps.build.definition",
	"nameSingular"->"BuildDefinition",
	"namePlural"->"BuildDefinitions",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["name"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/build/definitions?view=azure-devops-rest-5.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_build/definition?definitionId=`definitionId`",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/build/definitions/`definitionId`?api-version=5.0",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/build/definitions?api-version=5.0",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]],  
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"definitionId" -> res["id"],
		"name" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Build*)


<|
	"azType"->"devOps.build.run",
	"nameSingular"->"BuildRun",
	"namePlural"->"BuildRuns",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData}, (refData["repositoryName"] /. _Missing ->"" ) <> "  :  " <> refData["buildNumber"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/build/builds?view=azure-devops-rest-6.1",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/APIs/_build/results?buildId=`buildId`&view=results",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/build/builds/`buildId`?api-version=6.1-preview.6",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/build/builds?api-version=6.1-preview.6",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]],  
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"buildId" -> res["id"],
		"repositoryName" -> res["repository","name"],
		"buildNumber" -> res["buildNumber"]
	|>],
	"searchFields" -> {"buildNumber","id","repositoryName"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Artifacts*)


<|
	"azType"->"devOps.build.artifact",
	"nameSingular"->"BuilddArtifact",
	"namePlural"->"BuildArtifacts",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData}, refData["artifactName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/build/artifacts?view=azure-devops-rest-6.1",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_build/results?buildId=`buildI`d&view=artifacts&type=publishedArtifacts",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/build/builds/`buildId`/artifacts?api-version=6.1-preview.5",
	"listFilter" -> azRefDevOpsPattern["devOps.build.run"],
	"parentAzType" -> "devOps.build.run",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["resource","url"]],  
		"projectId" -> devOpsProjectIdFromUrl[res["resource","url"]],
		"artifactId" -> res["id"],
		"artifactName" -> res["name"],
		"artifactTyoe" -> res["resource","type"],
		"downloadUrl" ->res["resource","downloadUrl"]
	|>],
	"searchFields" -> {"buildNumber","id","repositoryName"}
|> // devOpsDefaultOperationsBuilder

azDownload[auth_, azRefDevOpsPattern["devOps.build.artifact"]] := 
	azDownload[auth, ref, FileNameJoin[{ $HomeDirectory,"Downloads",ref["artifactName"]<>".zip"}]];

azDownload[auth_, azRefDevOpsPattern["devOps.build.artifact"], file_String] := 
	azDownload[auth, ref["downloadUrl"], file];
	
azFileNames[auth_, azRefDevOpsPattern["devOps.build.artifact"]] :=
	azFileNames[auth, ref["downloadUrl"]];


(* ::Subsection::Closed:: *)
(*Release*)


(* ::Subsubsection::Closed:: *)
(*Release definition*)


<|
	"azType"->"devOps.releaseDefinition",
	"nameSingular"->"ReleaseDefinition",
	"namePlural"->"ReleaseDefinitions",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["definitionName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/release/definitions?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_release?definitionId=`definitionId`",
	"getUrl"->"https://vsrm.dev.azure.com/`organizationName`/`projectId`/_apis/release/definitions/`definitionId`?api-version=6.0-preview.4",
	"listUrl" -> "https://vsrm.dev.azure.com/`organizationName`/`projectName`/_apis/release/definitions?api-version=5.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"definitionName" -> res["name"],
		"definitionId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Release*)


(* ::Text:: *)
(*https://stackoverflow.com/questions/57658663/how-to-start-a-specific-stage-in-a-release-using-azure-devops-rest-api*)


<|
	"azType"->"devOps.release.run",
	"nameSingular"->"ReleaseRun",
	"namePlural"->"ReleaseRuns",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData}, refData["releaseDefinition"] <> "/" <> ToString@refData["releaseName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/release/releases?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_release?releaseId=`releaseId`&_a=release-summary",
	"getUrl"->"https://vsrm.dev.azure.com/`organizationName`/`projectId`/_apis/release/releases/`releaseId`?api-version=6.0-preview.8",
	"listUrl" -> "https://vsrm.dev.azure.com/`organizationName`/`projectId`/_apis/release/releases?definitionId=`definitionId`&api-version=6.0-preview.8",
	"listFilter" -> azRefDevOpsPattern["devOps.releaseDefinition"],
	"parentAzType" -> "devOps.releaseDefinition",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"releaseDefinition" -> res["releaseDefinition","name"],
		"releaseId" -> res["id"],
		"releaseName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder

azDevOpsReleaseRunCreate[authorizationHeader_String, azRefDevOpsPattern["devOps.releaseDefinition"]] :=
	azDevOpsReleaseRunCreate[authorizationHeader, ref, <| "definitionId" -> refData["definitionId"] |>]
	
azDevOpsReleaseRunStages[auth_String, azRefDevOpsPattern["devOps.release"]] := 
	azInfo[auth, ref]["environments",All,{"id","name","status"}];
	
azDevOpsReleaseRunStageDeploy[auth_String, azRefDevOpsPattern["devOps.release"], stageId_Integer] := 
	azHttpPatch[
		auth,
		StringTemplate["https://vsrm.dev.azure.com/`organizationName`/`projectId`/_apis/Release/releases/`releaseId`/environments/`environmentId`?api-version=6.0-preview.7"][<| refData, "environmentId"->stageId|>],
		<|"status"->"inProgress"|>
	]


(* ::Subsection::Closed:: *)
(*Graph*)


(* ::Subsubsection::Closed:: *)
(*Users*)


<|
	"azType"->"devOps.user",
	"nameSingular"->"User",
	"namePlural"->"Users",
	"panelIcon"-> icons["devOps.user"],
	"panelLabelFunc"-> Function[{refData}, refData["displayName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/graph/users?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://`organizationName`.visualstudio.com/_settings/users",
	"getUrl"->"https://vssps.dev.azure.com/`organizationName`/_apis/graph/users/`userDescriptor`?api-version=6.0-preview.1",
	"listUrl" -> "https://vssps.dev.azure.com/`organizationName`/_apis/graph/users?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.organization"],
	"parentAzType" -> "devOps.organization",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"userDescriptor" -> res["descriptor"],
		"displayName" -> res["displayName"]
	|>],
	"searchFields" -> {"displayName","mailAddress","directoryAlias"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Users groups*)


<|
	"azType"->"devOps.group",
	"nameSingular"->"Group",
	"namePlural"->"Groups",
	"panelIcon"-> icons["devOps.user"],
	"panelLabelFunc"-> Function[{refData}, refData["displayName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/graph/groups?view=azure-devops-rest-6.0",
	"getUrl"->"https://vssps.dev.azure.com/`organizationName`/_apis/graph/groups/`groupDescriptor`?api-version=6.0-preview.1",
	"listUrl" -> "https://vssps.dev.azure.com/`organizationName`/_apis/graph/groups?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.organization"],
	"parentAzType" -> "devOps.organization",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"groupDescriptor" -> res["descriptor"],
		"displayName" -> res["displayName"]
	|>],
	"searchFields" -> {"displayName"}
|> // devOpsDefaultOperationsBuilder

azDevOpsGroupList[authorizationHeader_String, azRefDevOpsPattern["devOps.user"]] := Module[
	{memberships},
	memberships = azHttpGet[
		authorizationHeader,
		StringTemplate["https://vssps.dev.azure.com/`organizationName`/_apis/graph/Memberships/`userDescriptor`?api-version=6.0-preview.1"][
			URLEncode/@refData
		]]
]


(* ::Subsection:: *)
(*Artifacts*)


(* ::Subsubsection:: *)
(*Feed*)


<|
	"azType"->"devOps.artifact.feed",
	"nameSingular"->"ArtifactFeed",
	"namePlural"->"ArtifactFeeds",
	"panelIcon"-> icons["devOps.artifact"],
	"panelLabelFunc"-> Function[{refData}, refData["feedName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/artifacts/feed%20%20management?view=azure-devops-rest-6.1",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectName`/_packaging?_a=feed",
	"getUrl"->"https://feeds.dev.azure.com/`organizationName`/_apis/packaging/feeds/`feedId`?api-version=6.0-preview.1",
	"listUrl" -> "https://feeds.dev.azure.com/`organizationName`/_apis/packaging/feeds?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.organization"],
	"parentAzType" -> "devOps.organization",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]],  
		"feedId" -> res["id"],
		"feedName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder



(* ::Subsubsection:: *)
(*Packages*)


<|
	"azType"->"devOps.artifact.package",
	"nameSingular"->"ArtifactPackage",
	"namePlural"->"ArtifactPackages",
	"panelIcon"-> icons["devOps.artifact"],
	"panelLabelFunc"-> Function[{refData}, "(" <> refData["packageType"] <> ") " <> refData["packageName"] ],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/artifacts/feed%20%20management?view=azure-devops-rest-6.2",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectName`/_packaging?_a=feed",
	"getUrl"->"https://feeds.dev.azure.com/`organizationName`/_apis/packaging/Feeds/`feedId`/packages/`packageId`?api-version=6.0-preview.1",
	"listUrl" -> "https://feeds.dev.azure.com/`organizationName`/`projectName`/_apis/packaging/Feeds/`feedId`/packages?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.artifact.feed"],
	"parentAzType" -> "devOps.artifact.feed",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]],  
		"feedId" -> getIdKeyValue[res["url"],"feeds/"],
		"packageName" -> res["normalizedName"],
		"packageId" -> res["id"],
		"packageType" -> res["protocolType"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder

azFileNames[auth_, azRefDevOpsPattern["devOps.artifact.package"]] := Module[
	{latestVersion},
	latestVersion = azDevOpsArtifactPackageVersionList[auth,ref][SelectFirst[#isLatest&],"azRef"];
	azFileNames[auth,latestVersion]
]

azDownload[authorizationHeader_String, azRefDevOpsPattern["devOps.artifact.package"],args___] := Module[
	{latestVersion},
	latestVersion = azDevOpsArtifactPackageVersionList[authorizationHeader,ref][SelectFirst[#isLatest&],"azRef"];
	azDownload[authorizationHeader,latestVersion,args]
]	


(* ::Subsubsection:: *)
(*Package versions*)


<|
	"azType"->"devOps.artifact.version",
	"nameSingular"->"ArtifactPackageVersion",
	"namePlural"->"ArtifactPackageVersionss",
	"panelIcon"-> icons["devOps.artifact"],
	"panelLabelFunc"-> Function[{refData}, refData["packageName"]<>" : "<> refData["packageVersion"]],
	"restDocumentation"-> "https://docs.microsoft.com/en-us/rest/api/azure/devops/artifacts/artifact%20%20details?view=azure-devops-rest-6.1",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectName`/_packaging?_a=feed",
	"getUrl"->"https://feeds.dev.azure.com/`organizationName`/`projectName`/_apis/packaging/Feeds/`feedId`/Packages/`packageId`/versions/`packageVersion`?api-version=6.1-preview.1",
	"listUrl" -> "https://feeds.dev.azure.com/`organizationName`/`projectName`/_apis/packaging/Feeds/`feedId`/Packages/`packageId`/versions?api-version=6.1-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.artifact.package"],
	"parentAzType" -> "devOps.artifact.package",
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]],  
		"feedId" -> getIdKeyValue[res["url"],"feeds/"],
		"packageId" -> refData["packageId"],
		"packageName" -> refData["packageName"],
		"packageVersion" -> res["normalizedVersion"]
	|>],
	"searchFields" -> {"normalizedVersion"}
|> // devOpsDefaultOperationsBuilder;
 
azFileNames[auth_, azRefDevOpsPattern["devOps.artifact.version"]] := Module[
	{tempDir,files},
	tempDir = FileNameJoin[{$TemporaryDirectory,ToString@CreateUUID[]}];
	azDownload[auth, ref, tempDir];
	files=FileNames[All, tempDir, Infinity];
	DeleteDirectory[tempDir,DeleteContents->True];
	StringReplace[#,tempDir :> "", IgnoreCase->True ]& /@ files
]
	
azDownload[auth_, azRefDevOpsPattern["devOps.artifact.version"]] := 
	azDownload[auth, ref, FileNameJoin[{ $HomeDirectory,"Downloads",ref["packageName"]<>"_"<>ref["packageVersion"]}]];

azDownload[_String, azRefDevOpsPattern["devOps.artifact.version"], path_String] :=
	RunProcess[{
		$azExe, "artifacts", "universal", "download",
		"--organization","https://dev.azure.com/"<>refData["organizationName"],
		"--feed", refData["feedId"],
		"--name", refData["packageName"], 
		"--version",refData["packageVersion"],
		"--path",path}] /. KeyValuePattern["ExitCode" -> 0] -> path


(* ::Section::Closed:: *)
(*Footer*)


End[];


EndPackage[]
