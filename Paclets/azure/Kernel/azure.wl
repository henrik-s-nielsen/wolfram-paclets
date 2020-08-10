(* ::Package:: *)

(* ::Title:: *)
(*Azure*)


(* ::Section::Closed:: *)
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

azSetUiPortalPrefix;
azGetUiPortalPrefix

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

(* Event Hubs *)
azEventHubs;
azEventHubNamespaces;

(* Monitor: activity log *)
azActivityLog;

(* Azure DevOps *)
azDevOpsProjects;
azDevOpsProjectList;
azDevOpsGitRepositories;
azDevOpsGitRepositoryList;
azDevOpsPipelines;
azDevOpsPipelineList;
azDevOpsReleaseDefinitions;
azDevOpsReleaseDefinitionList;
azDevOpsReleases;
azDevOpsReleaseList;


(* temp *)
azRefDevOpsPattern;
azRefAzurePattern;
getIdKeyValue;
keyValuesFromId;
refLabel;


Begin["`Private`"];


(* ::Section:: *)
(*Base*)


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

azInfo[authorizationHeader_String, azRefAzurePattern[\"`azType`\"]] := 
	azHttpGet[authorizationHeader,
		StringTemplate[\"`getUrl`\"][refValues@ref]] /. 
			ds_Dataset :> ds [<| \"azRef\" -> azRef@refData, # |>&];

az`pluralName`[args___] := azApiManagementServiceList[args] /. ds_Dataset :> Normal@ds[All,\"azRef\"];
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
	"pluralName" -> StringJoin@MapAt[Pluralize, config["name"], -1]
|>] 
]


(* ::Section:: *)
(*azRef panel*)


(* ::Input:: *)
(**)


(* Notebook define azIcons variable *)
NotebookImport[DirectoryName@FindFile@"azure`"<>"icons.nb","Input"->"Expression"];

icons =  RemoveBackground@Image[#,ImageSize->15]& /@ azIcons;

refIcon[KeyValuePattern["azType" -> t_]] := Which[
	StringContainsQ[t, "devOps."], icons["devOps.default"],
	True, icons["azure.default"]
];
refLabel[KeyValuePattern["azType" -> _]] := "???";
refLabel[_] := "???";
refColor[KeyValuePattern["azType" -> t_]] := Which[
	StringContainsQ[t, "devOps."], Lighter[Green, 0.9],
	True, Lighter[Blue, 0.9]
];

refType[KeyValuePattern["azType"->type_String]] := Last@StringSplit[type,"."] /; 
	StringContainsQ[type,"."];
refType[_] := "unkown type";

azRef /: MakeBoxes[ref:azRef[refData_Association], fmt_] :=
Block[{type, bg, display},
	bg = refColor[refData];
	type =  refType[refData];
	display = Tooltip[
		Framed[
			Row[{refIcon[refData]," ",Style[type, Italic], ": ", Style[refLabel[refData],Bold]}],
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


refLabel[ref:KeyValuePattern["azType" -> "azure.aksCluster"]] := ref["aksName"];
refIcon[KeyValuePattern["azType" -> "azure.aksCluster"]] := icons["azure.aks"];

azOpenUi[azRefAzurePattern["azure.aksCluster"]] := Module[{url},
	url = StringTemplate["https://portal.azure.com/#@santander.onmicrosoft.com/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ContainerService/managedClusters/`aksName`/overview"]
		[URLEncode /@ refData];
	azOpenUi[url];
];

azInfo[authorizationHeader_String, azRefAzurePattern["azure.aksCluster", <||>]] := 
	azHttpGet[authorizationHeader,
		StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ContainerService/managedClusters/`aksName`?api-version=2020-06-01"][URLEncode/@ref]
	] /. ds_Dataset :> ds [<| "azRef" -> azRef@refData, # |>&];

azAksClusters[args___] := azAksClusterList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azAksClusterList[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"subscriptionId" -> _String}]]
] := azHttpGetPaged[
	authorizationHeader, 
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.ContainerService/managedClusters?api-version=2020-06-01"][ref]]  /.
		ds_Dataset :> ds[All, <|"azRef"-> azRef[<|
			"azType" -> "azure.aksCluster",
			getIdKeyValue[#["id"], "subscriptions", "subscriptionId"],
			getIdKeyValue[#["id"], "resourcegroups", "resourceGroupName"],
			"aksName" -> #["name"]
		|>], #|> &] 


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


azLogAnalyticsTableStatistics[authorizationHeader_String,ref_] := azLogAnalyticsTableStatistic[authorizationHeader,ref, {Now-Quantity[24,"Hours"],Now}];
azLogAnalyticsTableStatistics[authorizationHeader_String,ref:azRef[KeyValuePattern["azType"->"azure.logAnalyticsWorkspace"]], dateRange:{_DateObject,_DateObject}] :=
	azLogAnalyticsQuery[authorizationHeader, ref,"
		Usage
		| where IsBillable == true
	", dateRange] /. ds_Dataset :> ds["Table_0",GroupBy["DataType"],Total /* (UnitConvert[#,Quantity[1,"Gigabytes"]]&),Quantity[#Quantity,#QuantityUnit]&][ReverseSort]


(* ::Subsection:: *)
(*Workspaces*)


refLabel[ref:KeyValuePattern["azType" -> "azure.logAnalyticsWorkspace"]] := ref["workspaceName"];
refIcon[KeyValuePattern["azType" -> "azure.logAnalyticsWorkspace"]] := icons["azure.logAnalyticsWorkspace"];

azOpenUi[azRefAzurePattern["azure.logAnalyticsWorkspace"]] := Module[{url},
	url = StringTemplate["https://portal.azure.com/#@santander.onmicrosoft.com/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`"]
		[URLEncode /@ refData];
	azOpenUi[url];
];

azInfo[authorizationHeader_String, azRefAzurePattern["azure.logAnalyticsWorkspace", <||>]] := 
	azHttpGet[authorizationHeader,
		StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`?api-version=2020-03-01-preview"][URLEncode/@ref]
	] /. ds_Dataset :> ds [<| "azRef" -> azRef@refData, # |>&];

azLogAnalyticsWorkspaces[args___] := azLogAnalyticsWorkspaceList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azLogAnalyticsWorkspaceList[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"subscriptionId" -> _String}]]
] := azHttpGet[
	authorizationHeader, 
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.OperationalInsights/workspaces?api-version=2020-03-01-preview"][ref]]  /.
		ds_Dataset :> ds["value", All, <|"azRef"-> azRef[<|
			"azType" -> "azure.logAnalyticsWorkspace",
			getIdKeyValue[#["id"], "subscriptions", "subscriptionId"],
			getIdKeyValue[#["id"], "resourcegroups", "resourceGroupName"],
			"workspaceName" -> #["name"] 
		|>], #|> &] 


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
(*kubernetes*)


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


(* ::Section:: *)
(*API manager*)


(* ::Subsection:: *)
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
	"azLabel"->"refData[\"serviceName\"]"
|> // stdAzureResource //ToExpression



(* ::Subsection:: *)
(*Loggers*)


typeApimServiceLogger="azure.ApiManagement/service.logger";

azApiManagementServiceLoggers[
	authorizationHeader_String,
	azRefAzurePattern[typeApimService]
] := azHttpGetPaged[
	authorizationHeader,
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/loggers?api-version=2019-12-01"][refValues@ref]
]  /. ds_Dataset :> ds[All, <| "azRef" -> azRef[<|
	"azType" -> typeApimServiceLogger,
	getIdKeyValue[#["id"], "subscriptions", "subscriptionId"],
	getIdKeyValue[#["id"], "resourcegroups", "resourceGroupName"],
	getIdKeyValue[#["id"], "service", "serviceName"],
	getIdKeyValue[#["id"], "loggers", "loggerName"]
|>] , # |> &];


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


(* ::Subsubsection::Closed:: *)
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
		"organizationName" -> organizationName_String, 
		"projectName"->projectName_String,
		asc
	|>},
	refObj:azRef[ref:KeyValuePattern@Normal@inner]
]


(* ::Subsection:: *)
(*Projects*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.project"]] := ref["projectName"];
refIcon[KeyValuePattern["azType" -> "devOps.release"]] := icons["devOps.project"];

azOpenUi[azRefDevOpsPattern["devOps.project"]] := Module[{url},
	url = StringTemplate["https://dev.azure.com/`organizationName`/`projectName`"]
		[URLEncode /@ ref];
	azOpenUi[url];
];

azInfo[authorizationHeader_String, azRefDevOpsPattern["devOps.project"]] := 
	azHttpGet[authorizationHeader,
		StringTemplate["https://dev.azure.com/`organizationName`/_apis/projects/`projectName`?api-version=6.0-preview.4"][URLEncode/@ref]
	] /. ds_Dataset :> ds[<| "azRef" -> azRef@ref, # |>&];

azDevOpsProjects[args___] := azDevOpsProjectList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsProjectList[authorizationHeader_String, azRef[ref:KeyValuePattern[{
	"organizationName" -> organization_String
}]]] := 
	azHttpGet[authorizationHeader, 
		StringTemplate["https://dev.azure.com/``/_apis/projects?api-version=2.0"][URLEncode@organization]] /. 
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.project",
				"organizationName" -> organization,
				"projectName" -> #["name"]
			|>], #|> &]


(* ::Subsection:: *)
(*Git*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.repository"]] := ref["repositoryName"];
refIcon[KeyValuePattern["azType" -> "devOps.repository"]] := icons["devOps.repository"];

azOpenUi[azRefDevOpsPattern["devOps.repository"]] := Module[{url},
	url = StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_git/`repositoryName`"]
		[URLEncode /@ ref];
	azOpenUi[url];
];

azInfo[authorizationHeader_String, azRefDevOpsPattern["devOps.repository", <|"repositoryName" -> _String|>]] := 
	azHttpGet[authorizationHeader,
		StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_apis/git/repositories/`repositoryId`?api-version=6.0-preview.1"][URLEncode/@ref]
	] /. ds_Dataset :> ds[<| "azRef" -> azRef@ref, # |>&];

azDevOpsGitRepositories[args___] := azDevOpsGitRepositoryList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsGitRepositoryList[authorizationHeader_String, azRefDevOpsPattern["devOps.project"]] := 
	azHttpGet[authorizationHeader, 
		StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_apis/git/repositories?api-version=6.0-preview.1"][URLEncode/@ref]] /. 
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.repository",
				"organizationName" -> organizationName,
				"projectName" -> projectName,
				"repositoryName" -> #["name"],
				"repositoryId" -> #["id"]		
			|>], #|> &]


(* ::Subsection:: *)
(*Pipelines*)


(* ::Subsubsection:: *)
(*Pipeline*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.pipeline"]] := ref["pipelineName"];
refIcon[KeyValuePattern["azType" -> "devOps.pipeline"]] := icons["devOps.pipeline"];

azOpenUi[azRefDevOpsPattern["devOps.pipeline", <|"pipelineId" -> _Integer|>]] := Module[{url},
	url = StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_build?definitionId=`pipelineId`"]
		[URLEncode /@ ref];
	azOpenUi[url];
];

azInfo[authorizationHeader_String, azRefDevOpsPattern["devOps.pipeline", <|"pipelineId" -> _Integer|>]] := 
	azHttpGet[authorizationHeader,
		StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_apis/pipelines/`pipelineId`?api-version=6.0-preview.1"][URLEncode/@ref]
	] /. ds_Dataset :> ds[<| "azRef" -> azRef@ref, # |>&];

azDevOpsPipelines[args___] := azDevOpsPipelineList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsPipelineList[authorizationHeader_String, azRefDevOpsPattern["devOps.project"]] :=
	azHttpGet[authorizationHeader, 
		StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_apis/pipelines?api-version=6.0-preview.1"][URLEncode/@ref]] /.
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.pipeline",
				"organizationName" -> organizationName,
				"projectName" -> projectName,
				"pipelineName" -> #["name"],
				"pipelineId" -> #["id"]
			|>], #|> &]


(* ::Subsubsection:: *)
(*Release definition*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.releaseDefinition"]] := ref["definitionName"];
refIcon[KeyValuePattern["azType" -> "devOps.releaseDefinition"]] := icons["devOps.pipeline"];

azOpenUi[azRefDevOpsPattern["devOps.releaseDefinition", <|"definitionId" -> _Integer|>]] := Module[{url},
	url = StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_release?definitionId=`definitionId`"]
		[URLEncode /@ ref];
	azOpenUi[url];
];

azInfo[authorizationHeader_String, azRefDevOpsPattern["devOps.releaseDefinition", <|"definitionId" -> _Integer|>]] := 
	azHttpGet[authorizationHeader,
		StringTemplate["https://vsrm.dev.azure.com/`organizationName`/`projectName`/_apis/release/definitions/`definitionId`?api-version=6.0-preview.4"][URLEncode/@ref]
	] /. ds_Dataset :> ds[<| "azRef" -> azRef@ref, # |>&];

azDevOpsReleaseDefinitions[args___] := azDevOpsReleaseDefinitionList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsReleaseDefinitionList[authorizationHeader_String, azRefDevOpsPattern["devOps.project"]] :=
	azHttpGet[authorizationHeader, 
		StringTemplate["https://vsrm.dev.azure.com/`organizationName`/`projectName`/_apis/release/definitions?api-version=5.1"][URLEncode/@ref]] /.
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.releaseDefinition",
				"organizationName" -> organizationName,
				"projectName" -> projectName,
				"definitionName" -> #["name"],
				"definitionId" -> #["id"]		
			|>], #|> &]


(* ::Subsubsection:: *)
(*Release*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.release"]] := ref["releaseDefinition"] <> "/" <> ToString@ref["releaseId"];
refIcon[KeyValuePattern["azType" -> "devOps.release"]] := icons["devOps.pipeline"];

azOpenUi[azRefDevOpsPattern["devOps.release", <|"releaseId" -> releaseId_|>]] := Module[{url},
	url = StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_release?releaseId=`releaseId`&_a=release-summary"]
		[URLEncode /@ ref];
	azOpenUi[url];
];

azInfo[authorizationHeader_String, azRefDevOpsPattern["devOps.release", <|"releaseId" -> releaseId_|>]] := 
	azHttpGet[authorizationHeader,
		StringTemplate["https://vsrm.dev.azure.com/`organizationName`/`projectName`/_apis/release/releases/`releaseId`?api-version=6.0-preview.8"][URLEncode/@ref]
	] /. ds_Dataset :> ds[<| "azRef" -> azRef@ref, # |>&];

azDevOpsReleases[args___] := azDevOpsReleaseList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsReleaseList[
	authorizationHeader_String, 
	azRefDevOpsPattern["devOps.releaseDefinition", <|"definitionId" -> definitionId_|>]
] :=
	azHttpGet[authorizationHeader, 
		StringTemplate["https://vsrm.dev.azure.com/`organizationName`/`projectName`/_apis/release/releases?definitionId=`definitionId`"][URLEncode/@ref]] /.
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.release",
				"organizationName" -> organizationName,
				"projectName" -> projectName,
				"releaseId" -> #["id"],
				"releaseDefinition" -> #["releaseDefinition","name"]
			|>], #|> &]


(* ::Section::Closed:: *)
(*Footer*)


End[];


EndPackage[]
