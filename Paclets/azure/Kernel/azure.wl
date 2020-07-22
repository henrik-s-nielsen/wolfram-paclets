(* ::Package:: *)

(* ::Title:: *)
(*Azure*)


(* ::Section:: *)
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

(* Azure DevOps *)
azDevOpsProjects;
azDevOpsGitRepositories;


Begin["`Private`"];


(* ::Section:: *)
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
(*DevOps*)


azDevOpsProjects[cnn:KeyValuePattern[{"authorizationHeader" -> _String, "organization" -> organization_String}]] := azHttpGet[cnn, 
	StringTemplate["https://dev.azure.com/``/_apis/projects?api-version=2.0"][URLEncode@organization]] /. ds_Dataset :> ds["value"];


azDevOpsGitRepositories[authorizationHeader_String, ref:KeyValuePattern[{"organization" -> organization_String, "project"->project_String}]] := azHttpGet[authorizationHeader, 
	StringTemplate["https://dev.azure.com/``/``/_apis/git/repositories?api-version=6.0-preview.1"][URLEncode@organization,URLEncode@project]] /. 
		ds_Dataset :> ds["value", All, <| "azRef" -> azRef[<|ref, "repository" -> #name |>], # |> &]


(* ::Section::Closed:: *)
(*Footer*)


End[];


EndPackage[]
