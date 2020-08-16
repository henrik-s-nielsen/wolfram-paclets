(* ::Package:: *)

(* ::Title:: *)
(*Azure*)


(* ::Section:: *)
(*Header*)


BeginPackage["azure`"];


(* Base *)
$azExe;
azShellLogin;
azHttpGet;
azHttpGetPaged;
azHttpPost;
azHttpPatch;
azShellGetToken;
azRef;
azParseRefFromId;
azInfo;
azOpenUi;
azRefToId;
azOperations;
azRestDocumentation;

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
azApiManagementServiceLoggers;
azApiManagementServiceLoggerList;
azApiManagementServiceSubscriptions;
azApiManagementServiceSubscriptionList;
azApiManagementServiceProducts;
azApiManagementServiceProductList;
azApiManagementServiceApis;
azApiManagementServiceApiList;
azApiManagementServiceApiSchemata;
azApiManagementServiceApiSchemaList;

(* Event Hubs *)
azEventHubs;
azEventHubNamespaces;

(* Monitor: activity log *)
azActivityLog;

(* Azure DevOps *)
azDevOpsProjects;
azDevOpsProjectList;
azDevOpsProjectSearch;
azDevOpsGitRepositories;
azDevOpsGitRepositoryList;
azDevOpsGitRepositorySearch;
azDevOpsBuildPipelines;
azDevOpsBuildPipelineList;
azDevOpsBuildPipelineSearch;
azDevOpsReleaseDefinitions;
azDevOpsReleaseDefinitionList;
azDevOpsReleaseDefinitionSearch;
azDevOpsReleases;
azDevOpsReleaseList;
azDevOpsReleaseSearch;
azDevOpsUsers;
azDevOpsUserList;
azDevOpsUserSearch;


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


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Base*)


pluralize[s_String] := Pluralize[s];


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

getIdKeyValue[id_String, idKey_String, outKey_String] := StringCases[
	id,
	idKey~~keyval:(Except["/"]..):>(keyval /. {{}:>Nothing, v_:>( outKey->v) }),
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


stdAzureResource[config_Association]:=Module[{}, StringTemplate["

refLabel[refData:KeyValuePattern[\"azType\"\[Rule] \"`azType`\"]] := `azLabel`;
refIcon[KeyValuePattern[\"azType\"\[Rule] \"`azType`\"]] := icons[\"`azIcon`\"];


azRefToId[azRefAzurePattern[\"`azType`\"]] :=
	StringTemplate[\"`idTemplate`\"][URLEncode /@ refData];

azOpenUi[azRefAzurePattern[\"`azType`\"]] := Module[{url},
	url = StringTemplate[\"`uiUrl`\"][refValues@ref];
	azOpenUi[url];
];

azRestDocumentation[azRefAzurePattern[\"`azType`\"]] :=
	SystemOpen[\"`restDocumentation`\"];

azInfo[authorizationHeader_String, azRefAzurePattern[\"`azType`\"]] := 
	azHttpGet[authorizationHeader,
		StringTemplate[\"`getUrl`\"][refValues@ref]] /. 
			ds_Dataset :> ds [<| \"azRef\" -> azRef@refData, # |>&];

az`pluralName`[args___] := az`listName`[args] /. ds_Dataset :> Normal@ds[All,\"azRef\"];
az`listName`[
	authorizationHeader_String,
	azRefAzurePattern[\"`listFilter`\"]
] := azHttpGetPaged[
	authorizationHeader,
	StringTemplate[\"`listUrl`\"][refValues@ref]
] /. ds_Dataset :> ds[All, <| \"azRef\" -> azRef[<|
	\"azType\" -> \"`azType`\",
	keyValuesFromId[#[\"id\"], \"`idTemplate`\"]
|>] , # |> &]

"][<|
	config,
	"listName" -> StringJoin[config["name"]]<>"List",
	"pluralName" -> StringJoin@MapAt[pluralize, config["name"], -1]
|>] 
]



devOpsPanelInfoBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"panelIcon" ->_Image,
	"panelLabelFunc"->_
}]] := TemplateObject[Hold[
panelInfo[TemplateSlot["azType"]] := <|
	"icon" -> TemplateSlot["panelIcon"],
	"labelFunc" -> TemplateSlot["panelLabelFunc"]
|>]][cfg] // ReleaseHold

devOpsOpenUiBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"uiUrl"-> _String
}]] := TemplateObject[Hold[
azOpenUi[azRefDevOpsPattern[TemplateSlot["azType"]]] := 
	azOpenDevOpsUi[StringTemplate[TemplateSlot["uiUrl"]][URLEncode /@ refData]]
]][cfg] // ReleaseHold

devOpsInfoBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"getUrl"->_String
}]] := TemplateObject[Hold[
azInfo[authorizationHeader_String, azRefDevOpsPattern[TemplateSlot["azType"]]] := 
	azHttpGet[authorizationHeader,
		StringTemplate[TemplateSlot["getUrl"]][URLEncode/@refData]
	] /. ds_Dataset :> ds[<| "azRef" -> ref, # |>&]
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
	"searchFields" -> {_String..}
}]] := TemplateObject[Hold[
Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"Search"][auth_String, TemplateSlot["listFilter"], pattern_] := Module[{query},
	query = Select[keyValueContainsQ[#, TemplateSlot["searchFields"], pattern] &];
	Sow[query];
	Symbol["azDevOps"<>TemplateSlot["nameSingular"]<>"List"][auth, ref][
		query
	]]
]][cfg] // ReleaseHold


devOpsDefaultOperationsBuilder[cfg_Association] := (
	devOpsPanelInfoBuilder[cfg];
	devOpsOpenUiBuilder[cfg];
	devOpsInfoBuilder[cfg];
	devOpsListBuilder[cfg];
	devOpsRestDocumentationBuilder[cfg];
	If[KeyExistsQ[cfg, "searchFields"],
		devOpsSearchBuilder[cfg],
		Null]
);
	


(* ::Section::Closed:: *)
(*azRef panel*)


(* ::Input:: *)
(**)


(* Notebook define azIcons variable *)
NotebookImport[DirectoryName@FindFile@"azure`"<>"icons.nb","Input"->"Expression"];
icons =  RemoveBackground@Image[#,ImageSize->15]& /@ azIcons;
panelInfo[_] := <|"icon"->icons["azure.default"],"labelFunc"->("???"&)|>

refType[KeyValuePattern["azType"->type_String]] := (Last@StringSplit[type,"."] // (Last@StringSplit[#,"/"] &)) /; 
	StringContainsQ[type,"."];
refType[_] := "unkown type";

azRef /: MakeBoxes[ref:azRef[refData_Association], fmt_] :=
Block[{type, bg, display, panelInf},
	panelInf = panelInfo[refData["azType"]];
	bg = Lighter[LightGreen];
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
		InterpretationBox[boxes, ref]
	]
]



(* ::Section::Closed:: *)
(*Subscriptions*)


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


<|
	"name"-> {"Log","Analytics","Workspace"},
	"idTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`",
	"getUrl" -> "https://management.azure.com`id`?api-version=2020-03-01-preview",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.OperationalInsights/workspaces?api-version=2020-03-01-preview",
	"listFilter" -> "azure.subscription",
	"uiUrl"->"/resource`id`/overview",
	"azType"->"azure.logAnalyticsWorkspace",
	"azIcon"->"azure.logAnalyticsWorkspace",
	"azLabel"->"refData[\"workspaceName\"]",
	"restDocumentation" -> "https://docs.microsoft.com/en-us/rest/api/loganalytics/workspaces"
|> // stdAzureResource //ToExpression



(* ::Subsection::Closed:: *)
(*Meta data*)


azLogAnalyticsWorkspaceMetadata[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"azType" -> "azure.logAnalyticsWorkspace"
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
		"azType" -> "azure.logAnalyticsWorkspace"
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


<|
	"name"-> {"Api","Management","Service"},
	"idTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`",
	"getUrl" -> "https://management.azure.com`id`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.ApiManagement/service?api-version=2019-12-01",
	"listFilter" -> "azure.subscription",
	"uiUrl"->"/resource`id`/overview",
	"azType"->"azure.ApiManagement/service",
	"azIcon"->"azure.apiManagement",
	"azLabel"->"refData[\"serviceName\"]",
	"restDocumentation" -> "https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apimanagementservice"
|> // stdAzureResource //ToExpression



(* ::Subsection::Closed:: *)
(*Loggers*)


<|
	"name"-> {"Api","Management","Service","Logger"},
	"idTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/loggers/`loggerId`",
	"getUrl" -> "https://management.azure.com`id`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/loggers?api-version=2019-12-01",
	"listFilter" -> "azure.ApiManagement/service",
	"uiUrl"->"/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/diagnostics",
	"azType"->"azure.ApiManagement/service/logger",
	"azIcon"->"azure.apiManagement",
	"azLabel"->"refData[\"loggerId\"]",
	"restDocumentation" -> "https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/logger"
|> // stdAzureResource //ToExpression



(* ::Subsection::Closed:: *)
(*Subscriptions*)


<|
	"name"-> {"Api","Management","Service","Subscription"},
	"idTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/subscriptions/`apiSubscriptionId`",
	"getUrl" -> "https://management.azure.com`id`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/subscriptions?api-version=2019-12-01",
	"listFilter" -> "azure.ApiManagement/service",
	"uiUrl"->"/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-subscriptions",
	"azType"->"azure.ApiManagement/service/subscription",
	"azIcon"->"azure.apiManagement",
	"azLabel"->"refData[\"apiSubscriptionId\"]",
	"restDocumentation" -> "https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/subscription"
|> // stdAzureResource //ToExpression

(* Due to subscription appearing twice in url we need to do custom handling *)
azApiManagementServiceSubscriptionList[
	authorizationHeader_String,
	azRefAzurePattern["azure.ApiManagement/service"]
] := azHttpGetPaged[
	authorizationHeader,
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/subscriptions?api-version=2019-12-01"][refValues@ref]
] /. ds_Dataset :> ds[All, <| "azRef" -> azRef[<|
	"azType" -> "azure.ApiManagement/service/subscription",
	"subscriptionId" -> refData["subscriptionId"],
	keyValuesFromId[#["id"], "/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/subscriptions/`apiSubscriptionId`"]
|>] , # |> &]


(* ::Subsection::Closed:: *)
(*Products*)


<|
	"name"-> {"Api","Management","Service","Product"},
	"idTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/products/`productName`",
	"getUrl" -> "https://management.azure.com`id`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/products?api-version=2019-12-01",
	"listFilter" -> "azure.ApiManagement/service",
	"uiUrl"->"/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-products",
	"azType"->"azure.ApiManagement/service/product",
	"azIcon"->"azure.apiManagement",
	"azLabel"->"refData[\"productName\"]",
	"restDocumentation" -> "https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/product"
|> // stdAzureResource //ToExpression


(* ::Subsection::Closed:: *)
(*API's*)


pluralize["Api"]="Apis";
<|
	"name"-> {"Api","Management","Service","Api"},
	"idTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiId`",
	"getUrl" -> "https://management.azure.com`id`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis?api-version=2019-12-01",
	"listFilter" -> "azure.ApiManagement/service",
	"uiUrl"->"/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"azType"->"azure.ApiManagement/service/api",
	"azIcon"->"azure.apiManagement",
	"azLabel"->"refData[\"apiId\"]",
	"restDocumentation" -> "https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apis"
|> // stdAzureResource //ToExpression


(* ::Subsection::Closed:: *)
(*API Schema's*)


<|
	"name"-> {"Api","Management","Service","Api","Schema"},
	"idTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiId`/schemas/`schemaId`",
	"getUrl" -> "https://management.azure.com`id`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiId`/schemas?api-version=2019-12-01",
	"listFilter" -> "azure.ApiManagement/service/api",
	"uiUrl"->"/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"azType"->"azure.ApiManagement/service/api/schema",
	"azIcon"->"azure.apiManagement",
	"azLabel"->"refData[\"schemaId\"]",
	"restDocumentation" -> "https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apischema"
|> // stdAzureResource //ToExpression


(* ::Section::Closed:: *)
(*Event Hubs*)


(* ::Subsubsection:: *)
(*Namespaces*)


(* azureIdToAzRef[{t"Microsoft.EventHub",sub_,resGrp_, rest_}]:=azRef[<|
	"type" \[Rule] t,
	"subscriptionId" \[Rule] sub,
	"resourceGroupName" \[Rule]resGrp
|>];
azRefToAzureId[ref_azRef] := 
	StringTemplate["/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/`type`/namespaces/ts-scb-test-apimgt-poc-ehub-ns"] *)

azEventHubNamespaces[authorizationHeader_String, azRef[ref:KeyValuePattern[{
		"subscriptionId" -> _String
	}]]
] := azHttpGet[
	authorizationHeader,
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.EventHub/namespaces?api-version=2017-04-01"][ref]
] /. ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|"namespace" -> #name, azParseRefFromId[#id]|>], # |>&]


(* ::Subsubsection:: *)
(*EventHubs*)


azEventHubs[authorizationHeader_String, azRef[ref:KeyValuePattern[{
		"subscriptionId" -> _String,
		"resourceGroupName" -> _String,
		"namespace" -> _String
	}]]
] := azHttpGet[
	authorizationHeader,
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.EventHub/namespaces/`namespace`/eventhubs?api-version=2017-04-01"][ref]
] /. ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|"eventhub" -> #name, ref|>], # |>&]


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
	"icon"->icons["devOps.default"],
	"labelFunc"->Function[{refData},refData["organizationName"]]
|>


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
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> res["id"],
		"projectName" -> res["name"] 
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder



(* ::Subsection::Closed:: *)
(*Git*)


<|
	"azType"->"devOps.repository",
	"nameSingular"->"GitRepository",
	"namePlural"->"GitRepositories",
	"panelIcon"-> icons["devOps.repository"],
	"panelLabelFunc"-> Function[{refData},refData["repositoryName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/git/repositories?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_git/`repositoryName`",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories/`repositoryId`?api-version=6.0-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/git/repositories?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"repositoryName" -> res["name"],
		"repositoryId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder



(* ::Subsection:: *)
(*Pipelines*)


(* ::Subsubsection::Closed:: *)
(*Build pipeline*)


<|
	"azType"->"devOps.buildPipeline",
	"nameSingular"->"BuildPipeline",
	"namePlural"->"BuildPipelines",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["pipelineName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/pipelines/pipelines?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_build?definitionId=`pipelineId`",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/pipelines/`pipelineId`?api-version=6.0-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/pipelines?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]],  
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"pipelineName" -> res["name"],
		"pipelineId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


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


<|
	"azType"->"devOps.release",
	"nameSingular"->"Release",
	"namePlural"->"Releases",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData}, refData["releaseDefinition"] <> "/" <> ToString@refData["releaseName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/release/releases?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_release?releaseId=`releaseId`&_a=release-summary",
	"getUrl"->"https://vsrm.dev.azure.com/`organizationName`/`projectId`/_apis/release/releases/`releaseId`?api-version=6.0-preview.8",
	"listUrl" -> "https://vsrm.dev.azure.com/`organizationName`/`projectId`/_apis/release/releases?definitionId=`definitionId`",
	"listFilter" -> azRefDevOpsPattern["devOps.releaseDefinition"],
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"projectId" -> devOpsProjectIdFromUrl[res["url"]],
		"releaseDefinition" -> res["releaseDefinition","name"],
		"releaseId" -> res["id"],
		"releaseName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsection:: *)
(*Graph*)


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
	"listResultKeysFunc" -> Function[{res}, <|
		"organizationName" -> devOpsOrgFromUrl[res["url"]], 
		"userDescriptor" -> res["descriptor"],
		"displayName" -> res["displayName"]
	|>],
	"searchFields" -> {"displayName","mailAddress","directoryAlias"}
|> // devOpsDefaultOperationsBuilder

(*
azDevOpsUserSearch[auth_String, azRefDevOpsPattern["devOps.organization"], pattern_] :=
	azDevOpsUserList[auth, ref][
		Select[keyValueContainsQ[#,{"displayName","mailAddress","directoryAlias"}, pattern] &]
	]
	
*)


(* ::Section::Closed:: *)
(*Footer*)


End[];


EndPackage[]
