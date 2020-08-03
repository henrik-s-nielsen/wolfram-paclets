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
azHttpPost;
azHttpPatch;
azShellGetToken;
azRef;
azParseRefFromId;
azInfo;
azOpenUi;

azShellGetSubscriptions;
azShellGetSubscriptionList;

(* Log analytics *)
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
azApimServices;
azApimLoggers;

(* Event Hubs *)
azEventHubs;
azEventHubNamespaces;

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


Begin["`Private`"];


(* ::Section:: *)
(*Base*)


azShellLogin[] := RunProcess[{$azExe,"login"}] /. KeyValuePattern["ExitCode"->0] -> Null;

$azExe = "C:\\Program Files (x86)\\Microsoft SDKs\\Azure\\CLI2\\wbin\\az.cmd";

azParseRestResponde[res_HTTPResponse] := 
	res /. r:HTTPResponse[_,KeyValuePattern[{"StatusCode"->200}],___]:>Dataset@ImportByteArray[r["BodyByteArray"],"RawJSON"]


		
azShellGetToken[subscriptionId_String] := RunProcess[{$azExe, "account", "get-access-token"}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_,"--subscription" ->subscriptionId }] :> 
		ImportString[std,"RawJSON"] /. KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
			Module[{a},a=ImportByteArray[StringToByteArray@std,"RawJSON"];a["expiresOn"]=DateObject[a["expiresOn"]];a]

toIso8601[{from_,to_}] := toIso8601@from<>"/"<>toIso8601@to;
toIso8601[date_DateObject] := DateString[DateObject[date,TimeZone->"Zulu"],"ISODateTime"]<>"Z";

azParseRefFromId[id_String] := StringCases[ToLowerCase@id,
	"/subscriptions/" ~~ subscription:(Except["/"]..) ~~
	"/resourcegroups/"~~resourcegroups:(Except["/"]..) :> 
	<| "subscriptionId" -> subscription , "resourceGroupName" -> resourcegroups |>
] /. {v_} :> v;

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
	idKey~~"/"~~keyval:(Except["/"]..):>(keyval /. {{}:>Nothing, v_:>( outKey->v) }),
	IgnoreCase->False
]/. {v_} :> v


azRefAzurePattern[azType_String] := azRefAzurePattern[azType, <||>];
azRefAzurePattern[azType_String,asc_Association] := With[
	{inner = <|
		"azType"->azType,
		"subscriptionId"-> subscriptionId_String,
		asc
	|>},
	refObj:azRef[ref:KeyValuePattern@Normal@inner]
]


azOpenUi[url_String] := Module[{},
	Sow[url];
	SystemOpen[url];
];
azOpenUi[KeyValuePattern["azRef" -> ref_]] := azOpenUi@ref;
azOpenUi[ds_Dataset] := azOpenUi@Normal@ds;


(* ::Section::Closed:: *)
(*azRef panel*)


(* ::Input:: *)
(**)


(* Notebook define azIcons variable *)
NotebookImport[DirectoryName@FindFile@"azure`"<>"icons.nb","Input"->"Expression"];

icons =  RemoveBackground@Image[#,ImageSize->15]& /@ azIcons;

icon[KeyValuePattern["azType" -> t_]] := Which[
	StringContainsQ[t, "devOps."], icons["devOps.default"],
	True, icons["azure.default"]
];
refLabel[KeyValuePattern["azType" -> _]] := "???";
refLabel[_] := "???";
refColor[KeyValuePattern["azType" -> t_]] := Which[
	StringContainsQ[t, "devOps."], Lighter[Green, 0.9],
	True, Lighter[Blue, 0.9]
];

azRefType[KeyValuePattern["azType"->type_String]] := Last@StringSplit[type,"."] /; 
	StringContainsQ[type,"."];
azRefType[_] := "unkown type";

azRef /: MakeBoxes[obj:azRef[ref_Association], fmt_] :=
Block[{type, bg, display},
	bg = refColor[ref];
	type =  azRefType[ref];
	display = Tooltip[
		Framed[
			Row[{icon[ref]," ",Style[type, Italic], ": ", Style[refLabel[ref],Bold]}],
			Background -> bg,
			BaselinePosition -> Baseline,
			BaseStyle -> "Panel",
			FrameMargins -> {{5,5},{3,3}},
			FrameStyle -> GrayLevel[0.8],
			RoundingRadius -> 5
		],
		Grid[KeyValueMap[{Style[#1,Bold],#2}&,ref],Alignment->Left]
	];
	
	(* should probably recast this as a TemplateBox eventually *)
	With[{boxes = ToBoxes[display, fmt]},
		InterpretationBox[boxes, obj]
	]
]



(* ::Section:: *)
(*Subscriptions*)


refLabel[ref:KeyValuePattern["azType" -> "azure.subscription"]] := ref["subscriptionName"];
icon[KeyValuePattern["azType" -> "azure.subscription"]] := icons["azure.subscription"];

azOpenUi[azRefAzurePattern["azure.subscription"]] := Module[{url},
	url = StringTemplate["https://portal.azure.com/#@santander.onmicrosoft.com/resource/subscriptions/`subscriptionId`/overview"]
		[URLEncode /@ ref];
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


(* ::Section:: *)
(*Log analytics*)


(* ::Text:: *)
(*https://dev.loganalytics.io/documentation/Overview*)
(*https://docs.microsoft.com/en-us/azure/application-gateway/application-gateway-diagnostics*)
(*https://docs.microsoft.com/en-us/rest/api/loganalytics/dataaccess/metadata/get*)


(* ::Subsection:: *)
(*Workspaces*)


refLabel[ref:KeyValuePattern["azType" -> "azure.logAnalyticsWorkspace"]] := ref["workspaceName"];
icon[KeyValuePattern["azType" -> "azure.logAnalyticsWorkspace"]] := icons["azure.logAnalyticsWorkspace"];

azOpenUi[azRefAzurePattern["azure.logAnalyticsWorkspace"]] := Module[{url},
	url = StringTemplate["https://portal.azure.com/#@santander.onmicrosoft.com/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`"]
		[URLEncode /@ ref];
	azOpenUi[url];
];

azInfo[authorizationHeader_String, azRefAzurePattern["azure.logAnalyticsWorkspace", <||>]] := 
	azHttpGet[authorizationHeader,
		StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`?api-version=2020-03-01-preview"][URLEncode/@ref]
	] /. ds_Dataset :> ds [<| "azRef" -> azRef@ref, # |>&];

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
	azRef[law:KeyValuePattern[{
		"subscriptionId"->_String,
		"resourceGroupName"->_String, 
		"resourceName" ->  _String}]]
] := Module[ {url,req,res,body},
	url = StringTemplate[
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`resourceName`/api/metadata?api-version=2017-01-01-preview"][law];
	req = HTTPRequest[url, <| Method->"GET", "ContentType"->"application/json", "Headers"->{"Authorization"->authorizationHeader} |>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde[res]
]


(* ::Subsection::Closed:: *)
(*Query*)


azLogAnalyticsQuery[
	authorizationHeader_String,
	resource_azRef, 
	query_String
] := azLogAnalyticsQuery[authorizationHeader, resource, query, {DateObject[{1800}],DateObject[{3000}]}];
azLogAnalyticsQuery[
	authorizationHeader_String,
	resource_azRef,
	query_String,
	Null
] := azLogAnalyticsQuery[authorizationHeader, resource, query];
azLogAnalyticsQuery[
	authorizationHeader_String,
	azRef[resource:KeyValuePattern[{
		"subscriptionId"->_String,
		"resourceGroupName"->_String, 
		"resourceName" ->  _String
	}]], 
	query_String,
	dateRange:{from_DateObject,to_DateObject}
] := Module[ {url,req,res,body},
	Sow[{query,dateRange}];
	url = StringTemplate[
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`resourceName`/api/query?api-version=2017-01-01-preview"][resource];
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
(*API manager*)


azApimServices[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"subscriptionId" -> _String
	}]]
] := azHttpGet[
	authorizationHeader,
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.ApiManagement/service?api-version=2019-12-01"][ref]
] /. ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|"resourceName" -> #name, azParseRefFromId@#id |>] , # |> &]


azApimLoggers[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"subscriptionId" -> _String,
		"resourceGroupName" -> _String,
		"resourceName" -> _String
	}]]
] := azHttpGet[
	authorizationHeader,
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`resourceName`/loggers?api-version=2019-12-01"][ref]
]["value"]


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
		"organizationName" -> organizationName_String, 
		"projectName"->projectName_String,
		asc
	|>},
	refObj:azRef[ref:KeyValuePattern@Normal@inner]
]


(* ::Subsection:: *)
(*Projects*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.project"]] := ref["projectName"];
icon[KeyValuePattern["azType" -> "devOps.release"]] := icons["devOps.project"];

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


(* ::Subsection::Closed:: *)
(*Git*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.repository"]] := ref["repositoryName"];
icon[KeyValuePattern["azType" -> "devOps.repository"]] := icons["devOps.repository"];

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


(* ::Subsection::Closed:: *)
(*Pipelines*)


(* ::Subsubsection::Closed:: *)
(*Pipeline*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.pipeline"]] := ref["pipelineName"];
icon[KeyValuePattern["azType" -> "devOps.pipeline"]] := icons["devOps.pipeline"];

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


(* ::Subsubsection::Closed:: *)
(*Release definition*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.releaseDefinition"]] := ref["definitionName"];
icon[KeyValuePattern["azType" -> "devOps.releaseDefinition"]] := icons["devOps.pipeline"];

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


(* ::Subsubsection::Closed:: *)
(*Release*)


refLabel[ref:KeyValuePattern["azType" -> "devOps.release"]] := ref["releaseDefinition"] <> "/" <> ToString@ref["releaseId"];
icon[KeyValuePattern["azType" -> "devOps.release"]] := icons["devOps.pipeline"];

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
