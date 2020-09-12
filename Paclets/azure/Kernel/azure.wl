(* ::Package:: *)

(* ::Title:: *)
(*Azure*)


(* ::Section:: *)
(*Notes*)


(* ::Text:: *)
(*To do:*)
(*- rename 	list to resource*)


(* ::Section::Closed:: *)
(*Header*)


BeginPackage["azure`"];


(* Base *)
azRefresh;
azHttpGet;
azHttpGetPaged;
azHttpPost;
azHttpPut;
azHttpPatch;
azHttpDelete;
azShellGetToken;
azRef;
azCreateAzRef;
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
azDownloadFile;
azDownloadByteArray;
azFileNames;
azParent;
yamlImport;

(* UI portal setup *)
azSetUiPortalPrefix;
azGetUiPortalPrefix;

(* Shell *)
$azExe;
azShellLogin;
azShellGetSubscriptions;
azShellGetSubscriptionList;

(* Auzure kubernetes service *)
azAksClusters;
azAksClusterList;

(* Monitor *)
azActivityLog;
azDiagnosticSettings;
azLogProfiles;
azDataCollectionRules;

(* Log analytics *)
azLogAnalyticsTableHelp;
azLogAnalyticsTableStatistics;
azLogAnalyticsWorkspaces;
azLogAnalyticsWorkspaceList;
azLogAnalyticsWorkspaceSearch;

(* Log anlytics & resources - log query *)
azLogQuery;
azLogQueryInfo;
azLogMetadata;

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
azApiManagementApiRevisions;
azApiManagementApiRevisionList;
azApiManagementApiSchemas;
azApiManagementApiSchemaList;
azApiManagementApiSchemaSearch;
azApiManagementGateways;
azApiManagementGatewayList;
azApiManagementGatewaySearch;
azApiManagementGatewayHostnames;
azApiManagementGatewayHostnameList;
azApiManagementGatewayHostnameSearch;
azApiManagementDiagnostics;
azApiManagementDiagnosticList;
azApiManagementDiagnosticSearch;
azApiManagementApiOperations;
azApiManagementApiOperationList;
azApiManagementApiOperationSearch;
azApiManagementApiReleases;
azApiManagementApiReleaseList;
azApiManagementApiReleaseSearch;

(* Event Hubs *)
azEventHubs;
azEventHubNamespaces;
azEventHubNamespaceList;
azEventHubNamespaceSearch;

(* Application Insight *)
azAppInsightComponents;
azAppInsightComponentList;
azAppInsightComponentSearch;

(* Application Gateway *)
azAppGateways;
azAppGatewayList;
azAppGatewaySearch;

(* VirtualNetwork *)
azVirtualNetworks;
azVirtualNetworkList;

(* Azure DevOps - project *)
azDevOpsProjects;
azDevOpsProjectList;
azDevOpsProjectSearch;

(* Azure DevOps - git *)
azDevOpsGitRepositories;
azDevOpsGitRepositoryList;
azDevOpsGitRepositorySearch;
azDevOpsGitClone;
azDevOpsCreateGitRef;
azDevOpsGitRefs;
azDevOpsGitRefList;
azDevOpsGitRefSearch;
azDevOpsGitCommits;
azDevOpsGitCommitList;
azDevOpsGitCommitSearch:
azDevOpsGitCommitGraph;
azDevOpsGitCommitPlot;
azDevOpsGitFolders;
azDevOpsGitFiles;

(* Azure DevOps - build pipeline *)
azDevOpsBuildDefinitions;
azDevOpsBuildDefinitionCreate;
azDevOpsBuildDefinitionList;
azDevOpsBuildDefinitionSearch;
azDevOpsBuildDefinitionProcess;
azDevOpsBuildDefinitionProcessFile;
azDevOpsBuildRuns;
azDevOpsBuildRunList;
azDevOpsBuildRunSearch;
azDevOpsBuildArtifacts;
azDevOpsBuildArtifactList;

(* Azure DevOps - release pipeline *)
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

(* Azure DevOps - User and groups *)
azEntitlements;
azDevOpvStorageKey;
azDevOpsUsers;
azDevOpsUserList;
azDevOpsUserSearch;
azDevOpsGroups;
azDevOpsGroupList;
azDevOpsGroupSearch;

(* Azure DevOps - aritfacts *)
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
refToAzureId;


Begin["`Private`"];


(* ::Section:: *)
(*Base*)


(* ::Subsection::Closed:: *)
(*Core*)


SetAttributes[functionCatch,HoldAllComplete];
functionCatch[tag_String, expr_] := Catch[expr] /. fail:Failure[ _,_] :> MapAt[Prepend[#,"FunctionTag"->tag] &, fail, {2}];

assertPattern[value_,pattern_] := 
If[MatchQ[value,pattern],
	value,
	Throw[
		Failure["assertionFailure",<|
			"MessageTemplate"->"Value does not match expected pattern",
			"Pattern"->pattern,
			"Value"->value
		|>]
	]
];
assertPattern[pattern_] := OperatorApplied[assertPattern][pattern];


(* RunProcess[{"pip","install","pyyaml"}] *)

yamlImport[s_String] := 
	ExternalEvaluate[
		"Python",
		StringTemplate["
import yaml
yaml.load(\"\"\"``\"\"\")
		"][s]
];


azRefresh[auth_, ref_azRef] := azInfo[auth, ref] /. ds_Dataset :> ds["azRef"] ;


(* parseUrl[someUrl, "/resource/`resourceId`/group/`groupName`"]  *)

parseUrl[txt_String, strPattern_String] := parseUrl[
	txt,
	StringSplit[strPattern, "`" ~~ key : (Except["`"] ..) ~~ "`" :> Key@key],
	<||>
];
parseUrl[txt_String, {first_String, rest___}, result_Association] := If[
	StringMatchQ[txt, StartOfString ~~ first ~~ ___, IgnoreCase -> True],
	parseUrl[StringReplace[txt, StartOfString ~~ first :> "" ], {rest}, result],
	$Failed
];
parseUrl[txt_String, {Key[key_String], rest___}, result_Association] :=
	parseUrl[
		StringReplace[txt, (StartOfString ~~ Except["/"] ..) -> ""],
		{rest},
		Append[result, key -> StringCases[txt, (StartOfString ~~ Except["/"] ..)] /. {v_} :> v]
	];
parseUrl[_, {}, result_Association] := result;


relations={};
azRelations[]:=relations;


azRef/:Normal[azRef[a_Association]] := a;
azRef[a_Association][property_String] := a[property];

azParent[auth_, base_azRef, parentType_String] := Module[{dirtyParent, cleanParent},
	dirtyParent = Normal@base;
	dirtyParent["azType"] = parentType;
	dirtyParent = azRef[dirtyParent];
	cleanParent = azRefresh[auth, dirtyParent];
	cleanParent 
];

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

azDownloadByteArray[authorizationHeader_String,  url_String] := Module[
	{res, req},
	req = HTTPRequest[url, <|
		Method -> "GET",
		"ContentType" -> "application/zip",
		"Headers" -> {"Authorization" -> authorizationHeader}
	|>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	res["BodyByteArray"]
]

azDownloadFile[authorizationHeader_String, target_, file_String] := Module[{},
	azDownloadByteArray[authorizationHeader,target] /. b_BodyByteArray :> (
		BinaryWrite[file,b];
		Close[file]
	)
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


azRelationPlot[] := azRelationPlot[azRelations[]];
azRelationPlot[rel_List] := Module[{edges,vertices},
edges=(#/.{t1_->t2_,l_}:>Tooltip[(t1->t2),TableForm@l])&/@rel;
vertices=(#/.{t1_->t2_,_}:>Splice[{t1->Tooltip[azIcon[t1],t1],t2->Tooltip[azIcon[t2],t2]}])&/@rel;
GraphPlot[edges,DirectedEdges->True,VertexShape->vertices,VertexSize->0.2,GraphLayout->"RadialEmbedding"] /. 
	Arrowheads[_]-> Arrowheads[0.01]
]


(* ::Subsection:: *)
(*Azure*)


refToAzureId[auth_, ref_azRef] := azInfo[auth,ref] /. ds_Dataset :> ds["id"];


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
AppendTo[relations, {TemplateSlot["parentAzType"]->TemplateSlot["azType"], {"az"<>TemplateSlot["namePlural"],"az"<>TemplateSlot["nameSingular"]<>"List"}}];
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

azureParentBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"parentAzType"->_String
}]] := TemplateObject[Hold[
azParent[auth_, azRefDevOpsPattern[TemplateSlot["azType"]]] :=
	azParent[auth, ref, TemplateSlot["parentAzType"]];
AppendTo[relations, {TemplateSlot["azType"]->TemplateSlot["parentAzType"], {"azParent"}}];
]][cfg] // ReleaseHold


azureDefaultOperationsBuilder[cfg_Association] := Module[
	{res = {}},
	azurePanelInfoBuilder[cfg] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "uiUrl"],
		azureOpenUiBuilder[cfg],
		Null] // AppendTo[res,#] &;
	azureInfoBuilder[cfg] // AppendTo[res,#] &;
	azureListBuilder[cfg] // AppendTo[res,#] &;
	azureRestDocumentationBuilder[cfg] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "searchFields"],
		azureSearchBuilder[cfg],
		Null] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "parentAzType"],
		azureRelationBuilder[cfg]; devOpsParentBuilder[cfg],
		Null] // AppendTo[res,#] &;
	res
];



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
Symbol["azDevOps"<>TemplateSlot["namePlural"]][args___] := 
	Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"List"][args] /. ds_Dataset :> Normal@ds[All,"azRef"];
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
AppendTo[relations, {TemplateSlot["parentAzType"]->TemplateSlot["azType"], {"azDevOps"<>TemplateSlot["namePlural"],"azDevOps"<>TemplateSlot["nameSingular"]<>"List"}}];
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

devOpsParentBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"parentAzType"->_String
}]] := TemplateObject[Hold[
azParent[azRefDevOpsPattern[TemplateSlot["azType"]]] :=
	azParent[auth, ref, TemplateSlot["parentAzType"]];
AppendTo[relations, {TemplateSlot["azType"]->TemplateSlot["parentAzType"], {"azParent"}}];
]][cfg] // ReleaseHold



devOpsDefaultOperationsBuilder[cfg_Association] := Module[
	{res = {}},
	devOpsPanelInfoBuilder[cfg] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg,"uiUrl"],
		devOpsOpenUiBuilder[cfg],
		Null] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg,"getUrl"],
		devOpsInfoBuilder[cfg],
		Null] // AppendTo[res,#] &;
	devOpsListBuilder[cfg] // AppendTo[res,#] &;
	devOpsRestDocumentationBuilder[cfg] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "searchFields"],
		devOpsSearchBuilder[cfg],
		Null] // AppendTo[res,#] &;
	devOpsCreateBuilder[cfg] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg,"getUrl"],
		devOpsUpdateBuilder[cfg],
		Null] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg,"getUrl"],		
		devOpsDeleteBuilder[cfg],
		Null] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "parentAzType"],
		devOpsRelationBuilder[cfg]; devOpsParentBuilder[cfg],
		Null] // AppendTo[res,#] &;
	res
];


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
	"panelLabelFunc"-> Function[{refData}, If[
							KeyExistsQ[refData,"subscriptionName"], 
							refData["subscriptionName"],
							refData["subscriptionId"]	
						]],
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/overview"
|>;
azurePanelInfoBuilder[cfg];

azRefresh[auth_, azRef[refData:KeyValuePattern["azType" -> "azure.subscription"]]] :=
	KeySelect[refData,  MemberQ[{"azType","subscriptionName","subscriptionId"},# ]&] // azRef

azShellGetSubscriptions[args___] := azShellGetSubscriptionList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azShellGetSubscriptionList[] := RunProcess[{$azExe ,"account","list"}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
		Dataset[ImportByteArray[StringToByteArray@std,"RawJSON"]][
			All, <| "azRef" -> azRef[<|
				"azType" -> "azure.subscription",
				"subscriptionName" -> #["name"],
				"subscriptionId" -> #["id"]
			|>], # |> &]



(* ::Section:: *)
(*Azure kubernetes service*)


cfg = <|
	"azType"->"azure.Aks.Cluster",
	"nameSingular"->"AksCluster",
	"namePlural"->"AksClusters",
	"panelIcon"-> icons["azure.aks"],
	"panelLabelFunc"-> Function[{refData}, refData["clusterName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/aks/managedclusters",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ContainerService/managedClusters/`clusterName`/overview",
	"getUrl"-> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ContainerService/managedClusters/`clusterName`?api-version=2020-06-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.ContainerService/managedClusters?api-version=2020-06-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"clusterName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg];


(* ::Section:: *)
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


(* ::Subsection:: *)
(*Workspaces*)


cfg = <|
	"azType"->"azure.logAnalytics.workspace",
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

azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
(*Meta data*)


azLogMetadata[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"azType" -> "azure.logAnalytics.workspace"
}]]] :=  azHttpGet[
	authorizationHeader, 
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`/api/metadata?api-version=2017-01-01-preview"][ref]
]

azLogMetadata[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"azType" -> "azure.applicationInsight.component"
}]]] :=  Module[{req, res},
	req = HTTPRequest[
		StringTemplate["https://api.applicationinsights.io/v1/apps/`appId`/metadata"][ref],
		<|"Headers"->{"X-Api-Key"-> authorizationHeader}|>
	];
	Sow[req];
	res = URLRead@req;
	azParseRestResponde@res
]




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


(* ::Section:: *)
(*Application Gateway*)


cfg = <|
	"azType"->"azure.appGateway",
	"nameSingular"->"AppGateway",
	"namePlural"->"AppGateways",
	"panelIcon"-> icons["azure.applicaionGateway"],
	"panelLabelFunc"-> Function[{refData}, refData["gatewayName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/application-gateway/applicationgateways",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/applicationGateways/scb-apim-prd-apim-agw/overview",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/applicationGateways/`gatewayName`?api-version=2020-05-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Network/applicationGateways?api-version=2020-05-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"gatewayName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Section::Closed:: *)
(*Monitor*)


(* ::Subsection::Closed:: *)
(*Log Query*)


azLogQueryInfo[auth_, ref_azRef] := 
	azLogQuery[auth, ref, "union * | where TimeGenerated > ago(24h) | summarize count() by Type, TenantId"]


azLogQuery[
	authorizationHeader_String,
	azRef[refData:KeyValuePattern[{
		"azType" -> "azure.applicationInsight.component"
	}]], 
	query_String,
	dateRange:{from_DateObject,to_DateObject}
] := Module[ {url, req,res,body},
	Sow[{query,dateRange}];
	url = StringTemplate["https://api.applicationinsights.io/v1/apps/`appId`/query"][refData];
	body = ExportString[<|"query"->query, "timespan" -> toIso8601[dateRange]|>,"RawJSON"];
	req = HTTPRequest[url, <| 
		Method->"POST", 
		"Body"->body, 
		"ContentType"->"application/json", 
		"Headers"->{"X-Api-Key"-> authorizationHeader} 
	|>];
	Sow[req];
	res = URLRead@req ;
	Sow[res];
	azParseRestResponde@res /. ds_Dataset :> logResourceDS@ds
];	

azLogQuery[
	authorizationHeader_String,
	azRef[refData:KeyValuePattern[{
		"azType" -> "azure.logAnalytics.workspace"
	}]], 
	query_String,
	dateRange:{from_DateObject,to_DateObject}
] := azLogQuery[
	authorizationHeader,
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`/api/query?api-version=2017-01-01-preview"]
		[refData],
	query,
	dateRange
] // logAnalyticDS;		

azLogQuery[
	auth_String,
	ref_azRef, 
	query_String,
	dateRange:{from_DateObject,to_DateObject}
] := functionCatch["azLogQuery", Module[{resourceId}, 
	resourceId = refToAzureId[auth, ref] // assertPattern[_String];
	azLogQuery[
		auth,
		"https://management.azure.com" <>
			resourceId <> 
			"/providers/microsoft.insights/logs?api-version=2018-03-01-preview",
		query,
		dateRange
	] // logResourceDS
]]
 
azLogQuery[
	authorizationHeader_String,
	res_, 
	query_String
] := azLogQuery[authorizationHeader, res, query, {DateObject[{1800}],DateObject[{3000}]}];

azLogQuery[
	authorizationHeader_String,
	url_String, 
	query_String,
	dateRange:{from_DateObject,to_DateObject}
] := Module[ {req,res,body},
	Sow[{query,dateRange}];
	body = ExportString[<|"query"->query, "timespan" -> toIso8601[dateRange]|>,"RawJSON"];
	req = HTTPRequest[url, <| Method->"POST", "Body"->body, "ContentType"->"application/json", "Headers"->{"Authorization"-> authorizationHeader} |>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde[res] 
]

logResourceFieldValue[_,Null] := Null;
logResourceFieldValue["bool",False] := False;
logResourceFieldValue["bool",True] := True;
logResourceFieldValue["datetime", v_String] := DateObject[v ,TimeZone->"Zulu"];
logResourceFieldValue["long",v_] := v;
logResourceFieldValue["string", v_String] := v;
logResorceField[col_, valueStr_] := col["name"]->logResourceFieldValue[col["type"],valueStr];
logResourceRow[columns_List,rows_List] := MapThread[logResorceField,{columns,rows}] // Association;
logResourceTable[columns_List,rows_List] := logResourceRow[columns,#]& /@ rows;
logResourceDS[ds_Dataset] := #name -> logResourceTable[#columns, #rows]& /@ Normal[ds["tables"]] // Association // Dataset;
logResourceDS[v_] := v;

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
logAnalyticDS[v_] := v;


(* ::Subsection::Closed:: *)
(*Activity log*)


azActivityLog[auth_, ref_azRef] := 
	azActivityLog[auth, ref, {Now-Quantity[89,"Days"], Now}];

azActivityLog[
	authorizationHeader_String,
	ref:azRef[KeyValuePattern["azType" -> _String]],
	dateRange:{fromDate_DateObject, toDate_DateObject}
] := azActivityLog[
	authorizationHeader,
	ref,
	StringTemplate["eventTimestamp ge '`fromDate`' and eventTimestamp le '`toDate`' and resourceUri eq '`resourceUri`'"]
		[<|
			"fromDate" -> toIso8601@fromDate,
			"toDate" -> toIso8601@toDate,
			"resourceUri" -> refToAzureId[authorizationHeader, ref]
		|>]
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


(* ::Subsection::Closed:: *)
(*Diagnostic settings*)


azDiagnosticSettings[authorizationHeader_, ref_azRef] := Module[
	{resourceId},
	resourceId = refToAzureId[authorizationHeader, ref] // assertPattern[_String];
	azHttpGet[
		authorizationHeader,
		StringTemplate["https://management.azure.com``/providers/microsoft.insights/diagnosticSettings?api-version=2017-05-01-preview"]
			[resourceId]
	]	
]


(* ::Subsection::Closed:: *)
(*Log profiles*)


azLogProfiles[authorizationHeader_, azRefAzurePattern["azure.subscription"]] := 
	azHttpGet[
		authorizationHeader,
		{
			"https://management.azure.com/subscriptions/`subscriptionId`/providers/microsoft.insights/logprofiles?api-version=2016-03-01",
			ref
		}
	]	


(* ::Subsection::Closed:: *)
(*Data collection rules*)


azDataCollectionRules[authorizationHeader_, azRefAzurePattern["azure.subscription"]] := 
	azHttpGet[
		authorizationHeader,
		{
			"https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Insights/dataCollectionRules?api-version=2019-11-01-preview",
			ref
		}
	]	


(* ::Section:: *)
(*API manager*)


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*API's*)


cfg = <|
	"azType"->"azure.apiManagement.api",
	"nameSingular"-> "ApiManagementApi",
	"namePlural"-> "ApiManagementApis",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["apiDisplayName"]],
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
		"apiName" -> res["name"],
		"apiDisplayName" -> res["properties","displayName"]
		
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
(*API revision*)


panelInfo["azure.apiManagement.api.revision"] := <|
	"icon" -> icons["azure.apiManagement"],
	"labelFunc" -> (#["apiId"] &)
|>;
azIcon[azRefAzurePattern["azure.apiManagement.api.revision"]] := azIcon[refData["azType"]];
azIcon["azure.apiManagement.api.revision"] := icons["azure.apiManagement"];

azApiManagementApiRevisions[auth_, ref_] :=
	azApiManagementApiRevisionList[auth, ref] /. ds_Dataset :> Normal@ds[All,"azRef"];

azApiManagementApiRevisionList[auth_, azRefAzurePattern["azure.apiManagement.api"]] :=
	azHttpGet[
		auth, {
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/revisions?api-version=2019-12-01",
			ref
		}
	] /. ds_Dataset :> ds["value",All,<|
		"azRef"-> azRef[<|
			"azType" -> "azure.apiManagement.api.revision",
			"apiId" -> StringReplace[#["apiId"], "/apis/" -> ""]
		|>],
		#
	|>&];
	
AppendTo[relations, {"azure.apiManagement.api"->"azure.apiManagement.api.revision", {"azApiManagementApiRevisions","azApiManagementApiRevisionList"}}];


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*Gateways*)


cfg = <|
	"azType"->"azure.apiManagement.gateway",
	"nameSingular"-> "ApiManagementGateway",
	"namePlural"-> "ApiManagementGateways",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["gatewayName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/gateway",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-gateways",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/gateways/`gatewayName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/gateways?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.service"],
	"parentAzType" -> "azure.apiManagement.service",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"gatewayName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


azApiManagementApiList[auth_, azRefAzurePattern["azure.apiManagement.gateway"]] := 
	azHttpGet[
		auth, {
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/gateways/`gatewayName`/apis?api-version=2019-12-01",
			ref
		}
	] /. ds_Dataset :> ds["value"];
AppendTo[relations, {"azure.apiManagement.gateway"->"azure.apiManagement.api", {"azApiManagementApiList"}}];


(* ::Subsection:: *)
(*Gateway hosts*)


cfg = <|
	"azType"->"azure.apiManagement.gatewayHostname",
	"nameSingular"-> "ApiManagementGatewayHostname",
	"namePlural"-> "ApiManagementGatewayHostnames",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["hostName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/gatewayhostnameconfiguration",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-gateways",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/gateways/`gatewayName`/hostnameConfigurations/`hostName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/gateways/`gatewayName`/hostnameConfigurations?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.gateway"],
	"parentAzType" -> "azure.apiManagement.gateway",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"gatewayName" -> getIdKeyValue[res["id"],"gateways/"],
		"hostName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsection:: *)
(*Diagnostics*)


cfg = <|
	"azType"->"azure.apiManagement.diagnostic",
	"nameSingular"-> "ApiManagementDiagnostic",
	"namePlural"-> "ApiManagementDiagnostics",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["diagnosticName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/diagnostic",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/diagnostics",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/diagnostics/`diagnosticName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/diagnostics?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.service"],
	"parentAzType" -> "azure.apiManagement.service",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"diagnosticName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsection:: *)
(*API Operation*)


cfg = <|
	"azType"->"azure.apiManagement.api.operation",
	"nameSingular"-> "ApiManagementApiOperation",
	"namePlural"-> "ApiManagementApiOperations",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData},  refData["operationName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apioperation",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/operations/`operationId`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/operations?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.api"],
	"parentAzType" -> "azure.apiManagement.api",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> getIdKeyValue[res["id"],"apis/"],
		"operationName" -> res["properties","displayName"],
		"operationId" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsection:: *)
(*API Release*)


cfg = <|
	"azType"->"azure.apiManagement.api.release",
	"nameSingular"-> "ApiManagementApiRelease",
	"namePlural"-> "ApiManagementApiReleases",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData},  refData["releaseId"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apirelease",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/releases/`releaseId`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/releases?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.api"],
	"parentAzType" -> "azure.apiManagement.api",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"releaseId" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Section:: *)
(*Event Hubs*)


(* ::Subsubsection:: *)
(*Namespaces*)


cfg = <|
	"azType"->"azure.eventHub.namespace",
	"nameSingular"-> "EventHubNamespace",
	"namePlural"-> "EventHubNamespaces",
	"panelIcon"-> icons["azure.eventHub"],
	"panelLabelFunc"-> Function[{refData}, refData["namespace"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/eventhub/event-hubs-runtime-rest",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.EventHub/namespaces",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionName`/resourceGroups/`resourceGroupName`/providers/Microsoft.EventHub/namespaces/`namespace`?api-version=2018-01-01-preview",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.EventHub/namespaces?api-version=2018-01-01-preview",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"namespace" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg];


(* ::Section:: *)
(*Application Insights*)


(* ::Subsection:: *)
(*Components*)


cfg = <|
	"azType"->"azure.appInsight.component",
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
		"componentName" -> res["name"],
		"appId" -> Association[res["properties"]]["AppId"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg];


(* ::Section:: *)
(*Virtual Networks*)


(* ::Subsection:: *)
(*Virtual Networks*)


cfg = <|
	"azType"->"azure.virtualNetwork",
	"nameSingular"-> "VirtualNetwork",
	"namePlural"-> "VirtualNetworks",
	"panelIcon"-> icons["azure.virtualNetwork"],
	"panelLabelFunc"-> Function[{refData}, refData["networkName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/virtualnetwork/virtualnetworks",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/reourceGroupName/providers/Microsoft.Network/virtualNetworks/`networkName`/overview",
	"getUrl"-> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/virtualNetworks/`networkName`?api-version=2020-05-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Network/virtualNetworks?api-version=2020-05-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"networkName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


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


(* ::Subsection:: *)
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




(* ::Subsection:: *)
(*Git*)


(* ::Subsubsection:: *)
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
|> // devOpsDefaultOperationsBuilder;


azDevOpsGitClone[auth_, azRefDevOpsPattern["devOps.git.repository"], folder_String] := Module[
	{repoInfo},
	repoInfo = azInfo[auth, ref];
	RunProcess[{"git","clone",repoInfo["webUrl"],folder}] /.
		KeyValuePattern["ExitCode" -> 0] :> folder
];


(* ::Subsubsection:: *)
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
		"refName" -> StringReplace[ToString@res["name"],"refs/"->""],
		"commit" -> azRef[<|
			"azType" -> "devOps.git.commit",
			"organizationName" -> devOpsOrgFromUrl[res["url"]], 
			"projectId" -> devOpsProjectIdFromUrl[res["url"]],
			"repositoryId" -> getIdKeyValue[res["url"],"repositories/"],			
			"commitId" -> res["objectId"]
		|>]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder 

azDevOpsGitRefList[authorizationHeader_String, azRefDevOpsPattern["devOps.git.commit"]] := functionCatch["azDevOpsGitRefList", Module[
	{repo,commit},
	commit = ref;
	repo = ReplacePart[ref,{1,"azType"}->"devOps.git.repository"];
	
	If[MemberQ[azDevOpsGitCommits[ authorizationHeader,#]//assertPattern[{_azRef...}] ,commit], #, Nothing] & /@ 
		(azDevOpsGitRefs[authorizationHeader,repo]//assertPattern[{_azRef...}])
]];

AppendTo[relations, {"devOps.git.ref"->"devOps.git.commit", {"azDevOpsGitRefs","azDevOpsGitRefList"}}];


(* ::Subsubsection:: *)
(*Commits*)


(conf = <|
	"azType"->"devOps.git.commit",
	"nameSingular"->"GitCommit",
	"namePlural"->"GitCommits",
	"panelIcon"-> icons["devOps.repository"],
	"panelLabelFunc"-> Function[{refData},StringTake[ToString@refData["commitId"],6]],
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


(* ::Subsubsection::Closed:: *)
(*Folders*)


cfg = <|
	"azType"->"devOps.git.folder",
	"panelIcon"-> icons["devOps.repository"],
	"panelLabelFunc"-> Function[{refData},refData["path"]]
|>;
devOpsPanelInfoBuilder[cfg]

azDevOpsGitFolders[auth_, azRefDevOpsPattern["devOps.git.commit"]] := Module[
	{treeId, rootFolder},
	treeId = azInfo[auth, ref] /. ds_Dataset :> ds["treeId"];
	rootFolder = azCreateAzRef[ref, "devOps.git.folder",<|"treeId"->treeId, "path" -> "/"|>];
	{rootFolder} ~Join~ azDevOpsGitFolders[auth, rootFolder]
];

azDevOpsGitFolders[auth_, azRefDevOpsPattern["devOps.git.folder"]] := 
	azHttpGet[
		auth,
		StringTemplate["https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`/trees/`treeId`?api-version=6.1-preview.1"]
			[refData]
	]  /. res_Dataset :> With[
		{folders = Normal@res["treeEntries",Select[#["gitObjectType"]=="tree"&],azRef[<|
			"azType" -> "devOps.git.folder",
			"organizationName" -> devOpsOrgFromUrl[#["url"]], 
			"projectId" -> devOpsProjectIdFromUrl[#["url"]],
			"repositoryId" -> getIdKeyValue[#["url"],"repositories/"],
			"treeId" -> #["objectId"],
			"path" -> ref["path"] <> #["relativePath"]<>"/"
		|>] &]},
		folders ~ Join ~ Flatten[azDevOpsGitFolders[auth,#]& /@ folders]
	];
AppendTo[relations, {"devOps.git.commit"->"devOps.git.folder", {"azDevOpsGitFolders"}}];


(* ::Subsubsection::Closed:: *)
(*Files*)


cfg = <|
	"azType"->"devOps.git.file",
	"panelIcon"-> icons["devOps.repository"],
	"panelLabelFunc"-> Function[{refData},refData["path"]]
|>;
devOpsPanelInfoBuilder[cfg]

azDevOpsGitFiles[auth_, folders:{(_azRef)...}] :=
	Flatten[azDevOpsGitFiles[auth, #]& /@ folders]

azDevOpsGitFiles[auth_, azRefDevOpsPattern["devOps.git.folder"]] := 
	azHttpGet[
		auth,
		StringTemplate["https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`/trees/`treeId`?api-version=6.1-preview.1"]
			[refData]
	]  /. res_Dataset :> Normal@res["treeEntries",Select[#["gitObjectType"]=="blob"&],azRef[<|
			"azType" -> "devOps.git.file",
			"organizationName" -> devOpsOrgFromUrl[#["url"]], 
			"projectId" -> devOpsProjectIdFromUrl[#["url"]],
			"repositoryId" -> getIdKeyValue[#["url"],"repositories/"],
			"objectId" -> #["objectId"],
			"path" -> ref["path"] <> #["relativePath"]
		|>] &]; 
AppendTo[relations, {"devOps.git.folder"->"devOps.git.file", {"azDevOpsGitFiles"}}];

azDevOpsGitFiles[auth_, azRefDevOpsPattern["devOps.git.commit"]] := 
	Flatten[azDevOpsGitFiles[auth,#] & /@ azDevOpsGitFolders[auth, ref]];
AppendTo[relations, {"devOps.git.commit"->"devOps.git.file", {"azDevOpsGitFiles"}}];

azDownloadByteArray[auth_, azRefDevOpsPattern["devOps.git.file"]] := 
	azDownloadByteArray[
		auth, 
		StringTemplate["https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`/blobs/`objectId`?api-version=6.1-preview.1"]
			[URLEncode /@ Normal[ref]]
	];


(* ::Subsection:: *)
(*Build*)


(* ::Subsubsection:: *)
(*Definition*)


<|
	"azType"->"devOps.build.definition",
	"nameSingular"->"BuildDefinition",
	"namePlural"->"BuildDefinitions",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["definitionName"]],
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
		"definitionName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder;



azDevOpsGitRepositoryList[auth_, azRefDevOpsPattern["devOps.build.definition"]] := functionCatch["azDevOpsGitRepositoryList",Module[
	{def, repoRef, repoInfo},
	(def = azInfo[auth, ref]) // 
		Normal // assertPattern[KeyValuePattern["repository"->_]];

	repoRef = azRef@Normal@def["repository", <|
		"azType" -> "devOps.git.repository",
		"organizationName" -> devOpsOrgFromUrl[#url], 
		"projectId" -> devOpsProjectIdFromUrl[#url],
		"repositoryName" -> #name,
		"repositoryId" -> #id 	
	|> &] // assertPattern[_azRef];
	
	Dataset@List@Normal[azInfo[auth, repoRef] // assertPattern[_Dataset]]
]];
AppendTo[relations, {"devOps.build.definition"->"devOps.git.repository", {"azDevOpsGitRepositories","azDevOpsGitRepositoryList"}}];


azDevOpsGitRefList[auth_, azRefDevOpsPattern["devOps.build.definition"]] := functionCatch["azDevOpsGitRefList", Module[
	{definition, defaultBranch, repo, gitrefs, gitref},
	(definition = azInfo[auth, ref]) //
		Normal // assertPattern[KeyValuePattern["repository"->KeyValuePattern["defaultBranch"->_String]]];
	defaultBranch = StringReplace[definition["repository","defaultBranch"], "refs/"->""];
	repo = azDevOpsGitRepositories[auth, ref] /. {v_} :> v // 
		assertPattern[_azRef]; 
	gitrefs = azDevOpsGitRefs[auth, repo] //
		assertPattern[{_azRef...}];
	gitref = Select[gitrefs, StringMatchQ[#["refName"], defaultBranch , IgnoreCase->True]&] /. {v_} :> v //
		assertPattern[_azRef];
	Dataset@List@Normal@azInfo[auth, gitref]
]];
AppendTo[relations, {"devOps.build.definition"->"devOps.git.ref", {"azDevOpsGitRefs","azDevOpsGitRefList"}}];


azDevOpsBuildDefinitionProcessFile[auth_, azRefDevOpsPattern["devOps.build.definition"]] := functionCatch["azDevOpsBuildDefinitionProcessFile", Module[
	{gitRef, files, definition, yamlFile},
	(definition = azInfo[auth, ref]) //
		Normal // assertPattern[KeyValuePattern["process"->KeyValuePattern["yamlFilename"->_String]]];
	yamlFile = definition["process","yamlFilename"];
	gitRef = azDevOpsGitRefs[auth, ref] /. {v_} :> v //
		assertPattern[_azRef];
	files = azDevOpsGitFiles[auth, gitRef["commit"]] //
		assertPattern[{_azRef...}];
	Select[files, StringContainsQ[#["path"], yamlFile] & ] /. {v_} :> v
]];
AppendTo[relations, {"devOps.build.definition"->"devOps.git.file", {"azDevOpsBuildDefinitionProcessFile"}}];


azDevOpsBuildDefinitionProcess[auth_, azRefDevOpsPattern["devOps.build.definition"]] :=
	azDevOpsBuildDefinitionProcessFile[auth, ref] /. 
		f_azRef :> azDownloadByteArray[auth,f] /.
		b_ByteArray :> (ByteArrayToString[b] // yamlImport) /. 
		a_Association :> Dataset@a;


(* ::Subsubsection:: *)
(*Run*)


<|
	"azType"->"devOps.build.run",
	"nameSingular"->"BuildRun",
	"namePlural"->"BuildRuns",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData}, (ToString@refData["repositoryName"] /. _Missing ->"" ) <> "  :  " <> ToString@refData["buildNumber"]],
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
|> // devOpsDefaultOperationsBuilder;


azDevOpsBuildRunList[auth_, azRefDevOpsPattern["devOps.build.definition"]] :=
	azDevOpsBuildRunList[auth, azParent[auth,ref]][Select[#definition["id"] == ref["definitionId"] &] ];
	
AppendTo[relations, {"devOps.build.definition"->"devOps.build.run", {"azDevOpsBuildRuns","azDevOpsBuildRunList"}}];


(* ::Subsubsection:: *)
(*Artifacts*)


<|
	"azType"->"devOps.build.artifact",
	"nameSingular"->"BuildArtifact",
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

azDownloadFile[auth_, azRefDevOpsPattern["devOps.build.artifact"]] := 
	azDownloadFile[auth, ref, FileNameJoin[{ $HomeDirectory,"Downloads",ref["artifactName"]<>".zip"}]];

azDownloadByteArray[auth_, azRefDevOpsPattern["devOps.build.artifact"], file_String] := 
	azDownloadByteArray[auth, ref["downloadUrl"], file];
	
azFileNames[auth_, azRefDevOpsPattern["devOps.build.artifact"]] :=
	azFileNames[auth, ref["downloadUrl"]];


(* ::Subsection:: *)
(*Release*)


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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
		"releaseDefinition" -> ToString@res["releaseDefinition","name"],
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
(*Entitlements*)


azEntitlements[auth_,azRefDevOpsPattern["devOps.user"]] :=
	azHttpGet[
		auth,
		StringTemplate["https://vsaex.dev.azure.com/`organizationName`/_apis/userentitlements/`storageKey`?api-version=6.1-preview.3"]
			[<|refData, "storageKey" -> azDevOpvStorageKey[auth, ref]|>]
	]


(* ::Subsection:: *)
(*Graph*)


(* ::Subsubsection::Closed:: *)
(*Storage key*)


azDevOpvStorageKey[auth_, azRefDevOpsPattern["devOps.user"]] :=Module[{res},
	azInfo[auth, ref] /. ds_Dataset :> azHttpGet[auth, ds["_links","storageKey","href"]]["value"]
] 


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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
	"panelLabelFunc"-> Function[{refData}, "(" <> ToString@refData["packageType"] <> ") " <> ToString@refData["packageName"] ],
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
|> // devOpsDefaultOperationsBuilder;

azFileNames[auth_, azRefDevOpsPattern["devOps.artifact.package"]] := Module[
	{latestVersion},
	latestVersion = azDevOpsArtifactPackageVersionList[auth,ref][SelectFirst[#isLatest&],"azRef"];
	azFileNames[auth,latestVersion]
]

azDownloadByteArray[authorizationHeader_String, azRefDevOpsPattern["devOps.artifact.package"],args___] := Module[
	{latestVersion},
	latestVersion = azDevOpsArtifactPackageVersionList[authorizationHeader,ref][SelectFirst[#isLatest&],"azRef"];
	azDownloadByteArray[authorizationHeader,latestVersion,args]
]	


(* ::Subsubsection:: *)
(*Package versions*)


<|
	"azType"->"devOps.artifact.version",
	"nameSingular"->"ArtifactPackageVersion",
	"namePlural"->"ArtifactPackageVersionss",
	"panelIcon"-> icons["devOps.artifact"],
	"panelLabelFunc"-> Function[{refData}, ToString@refData["packageName"]<>" : "<> ToString@refData["packageVersion"]],
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
	azDownloadFile[auth, ref, tempDir];
	files=FileNames[All, tempDir, Infinity];
	DeleteDirectory[tempDir,DeleteContents->True];
	StringReplace[#,tempDir :> "", IgnoreCase->True ]& /@ files
]
	
azDownloadFile[auth_, azRefDevOpsPattern["devOps.artifact.version"]] := 
	azDownloadByteArray[auth, ref, FileNameJoin[{ $HomeDirectory,"Downloads",ref["packageName"]<>"_"<>ref["packageVersion"]}]];

azDownloadFile[_String, azRefDevOpsPattern["devOps.artifact.version"], path_String] :=
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
