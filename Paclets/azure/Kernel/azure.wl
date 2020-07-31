(* ::Package:: *)

(* ::Title:: *)
(*Azure*)


(* ::Section::Closed:: *)
(*Header*)


BeginPackage["azure`"];


(* Base *)
$azExe;
azLogin;
azHttpGet;
azGetToken;
azRef;
azGetSubscriptions;
azParseRefFromId;
azInfo;
azOpenUi;

(* Log analytics *)
azLogAnalyticsQuery;
azLogAnalyticsWorkspaceMetadata;
azLogAnalyticsWorkspaces;
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
g;


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Icons*)


(* ::Input:: *)
(**)


(* Notebook define azIcons variable *)
NotebookImport[DirectoryName@FindFile@"azure`"<>"icons.nb","Input"->"Expression"];

icons =  RemoveBackground@Image[#,ImageSize->15]& /@ azIcons;

iconName["devOps.pipeline"] := "devOps.pipeline";
iconName["devOps.releaseDefinition"] := "devOps.pipeline";
iconName["devOps.release"] := "devOps.pipeline";
iconName["devOps.repository"] := "devOps.repository";
iconName[type_] := "devOps.default";


(* ::Section::Closed:: *)
(*Base*)


azLogin[] := RunProcess[{$azExe,"login"}] /. KeyValuePattern["ExitCode"->0] -> Null;

$azExe = "C:\\Program Files (x86)\\Microsoft SDKs\\Azure\\CLI2\\wbin\\az.cmd";

azParseRestResponde[res_HTTPResponse] := 
	res /. r:HTTPResponse[_,KeyValuePattern[{"StatusCode"->200}],___]:>Dataset@ImportByteArray[r["BodyByteArray"],"RawJSON"]

azGetSubscriptions[] := RunProcess[{$azExe ,"account","list"}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
		Dataset[ImportByteArray[StringToByteArray@std,"RawJSON"]][All, <| "azRef" -> azRef[<|"subscriptionId" -> #id|>], # |> &]
		
azGetToken[subscriptionId_String] := RunProcess[{$azExe, "account", "get-access-token"}] /.
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

azureIdToAzRef[id_String] := 
	StringCases[id, 
		"subscriptions/" ~~ subscription:(Except["/"]..) ~~
		"/resourceGroups/" ~~ resourceGrp:(Except["/"]..) ~~ 
		"/providers/" ~~ provider:(Except["/"]..)~~"/" ~~ 
		rest:((Except["?"] | Except[EndOfString])..) :> {
			provider,subscription,resourceGrp,rest }, IgnoreCase->True] /. {v_} :> azureId2azRef@v;



(* ::Section::Closed:: *)
(*Log analytics*)


(* ::Text:: *)
(*https://dev.loganalytics.io/documentation/Overview*)
(*https://docs.microsoft.com/en-us/azure/application-gateway/application-gateway-diagnostics*)
(*https://docs.microsoft.com/en-us/rest/api/loganalytics/dataaccess/metadata/get*)


(* ::Subsection:: *)
(*List workspaces*)


azLogAnalyticsWorkspaces[
	authorizationHeader_String,
	azRef[ref:KeyValuePattern[{
		"subscriptionId" -> _String}]]
] := azHttpGet[
	authorizationHeader, 
	StringTemplate["https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.OperationalInsights/workspaces?api-version=2020-03-01-preview"][ref]] /.
		ds_Dataset :> ds["value", All, <|"azRef"-> azRef[<|"resourceName" -> #name, azParseRefFromId[#id] |>], #|> &]


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*kubernetes*)


azLogAnalyticsKubeContainerShortNames[auth_,ref_] :=
	azLogAnalyticsQuery[auth, ref, "KubePodInventory | distinct ContainerName "] /. ds_Dataset :> 
			Sort@DeleteDuplicates[Last@StringSplit[#,"/"]& /@ (Normal@ ds["Table_0",All,"ContainerName"] /. "" -> Nothing)];


azLogAnalyticsKubeContainerIds[auth_,ref_, containerNameFilter_String] := 
	azLogAnalyticsQuery[auth, ref, StringTemplate["KubePodInventory | where ContainerName has '``' | distinct ContainerID"][containerNameFilter]] /.
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


refLabel[ref_Association] := Switch[ref["azType"],
	"devOps.project", ref["projectName"],
	"devOps.repository", ref["repositoryName"],
	"devOps.pipeline", ref["pipelineName"],
	"devOps.releaseDefinition", ref["definitionName"],
	"devOps.release", ref["releaseDefinition"] <> "/" <> ToString@ref["releaseId"],
	_, "???"
];

azRef /: MakeBoxes[obj:azRef[ref_Association], fmt_] :=
Block[{type, bg, display},
	bg = Lighter[Blue, 0.9];
	type = Last@StringSplit[ref["azType"],"."];
	display = Tooltip[
		Framed[
			Row[{icons[iconName@ref["azType"]]," ",Style[type, Italic], ": ", Style[refLabel@ref,Bold]}],
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


(* ::Subsection::Closed:: *)
(*Projects*)


azDevOpsProjects[args___] := azDevOpsProjectList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsProjectList[authorizationHeader_String, azRef[ref:KeyValuePattern[{
	"organizationName" -> organization_String
}]]] := 
	azHttpGet[authorizationHeader, 
		StringTemplate["https://dev.azure.com/``/_apis/projects?api-version=2.0"][URLEncode@organization]] /. 
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.project",
				"organizationName" -> organization,
				"projectName" -> #name
			|>], #|> &]


(* ::Subsection::Closed:: *)
(*Git*)


azDevOpsGitRepositories[args___] := azDevOpsGitRepositoryList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsGitRepositoryList[authorizationHeader_String, azRefDevOpsPattern["devOps.project"]] := 
	azHttpGet[authorizationHeader, 
		StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_apis/git/repositories?api-version=6.0-preview.1"][URLEncode/@ref]] /. 
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.repository",
				"organizationName" -> organizationName,
				"projectName" -> projectName,
				"repositoryName" -> #name		
			|>], #|> &]


(* ::Subsection::Closed:: *)
(*Pipelines*)


azDevOpsPipelines[args___] := azDevOpsPipelineList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsPipelineList[authorizationHeader_String, azRefDevOpsPattern["devOps.project"]] :=
	azHttpGet[authorizationHeader, 
		StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_apis/pipelines?api-version=6.0-preview.1"][URLEncode/@ref]] /.
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.pipeline",
				"organizationName" -> organizationName,
				"projectName" -> projectName,
				"pipelineName" -> #name		
			|>], #|> &]


azDevOpsReleaseDefinitions[args___] := azDevOpsReleaseDefinitionList[args] /. ds_Dataset :> Normal@ds[All,"azRef"];
azDevOpsReleaseDefinitionList[authorizationHeader_String, azRefDevOpsPattern["devOps.project"]] :=
	azHttpGet[authorizationHeader, 
		StringTemplate["https://vsrm.dev.azure.com/`organizationName`/`projectName`/_apis/release/definitions?api-version=5.1"][URLEncode/@ref]] /.
			ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|
				"azType" -> "devOps.releaseDefinition",
				"organizationName" -> organizationName,
				"projectName" -> projectName,
				"definitionName" -> #name,
				"definitionId" -> #id		
			|>], #|> &]



azOpenUi[azRefDevOpsPattern["devOps.release", <|"releaseId" -> releaseId_|>]] := Module[{url},
	url = StringTemplate["https://dev.azure.com/`organizationName`/`projectName`/_release?releaseId=`releaseId`&_a=release-summary"]
		[URLEncode /@ ref];
	Sow[url];
	SystemOpen[url];
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
				"releaseId" -> #id,
				"releaseDefinition" -> #releaseDefinition["name"]
			|>], #|> &]


(* ::Section::Closed:: *)
(*Footer*)


End[];


EndPackage[]
