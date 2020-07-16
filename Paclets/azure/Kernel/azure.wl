(* ::Package:: *)

BeginPackage["azure`"];


$azExe;
azLogin;
azGetToken;
azLogAnalyticsQuery;
azLogAnalyticsMetadata;
azGetSubscriptions;
azLogAnalyticsKubeContainerShortNames;
azLogAnalyticsKubeContainerIds;
azLogAnalyticsContainerLogStatistics;
azLogAnalyticsContainerLog;


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Base*)


azLogin[] := RunProcess[{$azExe,"login"}] /. KeyValuePattern["ExitCode"->0] -> Null;

$azExe = "C:\\Program Files (x86)\\Microsoft SDKs\\Azure\\CLI2\\wbin\\az.cmd";

azParseRestResponde[res_HTTPResponse] := 
	res /. r:HTTPResponse[_,KeyValuePattern[{"StatusCode"->200}],___]:>Dataset@ImportByteArray[r["BodyByteArray"],"RawJSON"]

azGetSubscriptions[] := RunProcess[{$azExe ,"account","list"}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
		ImportByteArray[StringToByteArray@std,"RawJSON"] // Dataset
		
azGetToken[subscriptionId_String] := RunProcess[{$azExe, "account", "get-access-token"}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_,"--subscription" ->subscriptionId }] :> 
		ImportString[std,"RawJSON"] /. KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
			Module[{a},a=ImportByteArray[StringToByteArray@std,"RawJSON"];a["expiresOn"]=DateObject[a["expiresOn"]];a]

tokenValue[token_String] := token;
tokenValue[KeyValuePattern["accessToken" -> token_String]] := token;


(* ::Section:: *)
(*Log analytics*)


(* ::Text:: *)
(*https://dev.loganalytics.io/documentation/Overview*)
(*https://docs.microsoft.com/en-us/azure/application-gateway/application-gateway-diagnostics*)
(*https://docs.microsoft.com/en-us/rest/api/loganalytics/dataaccess/metadata/get*)


(* ::Subsection::Closed:: *)
(*Meta data*)


azLogAnalyticsMetadata[
	cnn:KeyValuePattern[{
		"subscriptionId"->_String,
		"resourceGroupName"->_String, 
		"resourceName" ->  _String, 
		"accessToken"->token_}]
] := Module[ {url,req,res,body},
	url = StringTemplate[
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`resourceName`/api/metadata?api-version=2017-01-01-preview"][cnn];
	req = HTTPRequest[url, <| Method->"GET", "ContentType"->"application/json", "Headers"->{"Authorization"->"Bearer " <> tokenValue@token} |>];
	Sow[req];
	res = URLRead@req;
	Sow[res];
	azParseRestResponde@res
]


(* ::Subsection::Closed:: *)
(*Query*)


azLogAnalyticsQuery[
	cnn:KeyValuePattern[{
		"subscriptionId"->_String,
		"resourceGroupName"->_String, 
		"resourceName" ->  _String, 
		"accessToken"->token_}], 
	query_String
] := Module[ {url,req,res,body},
	url = StringTemplate[
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`resourceName`/api/query?api-version=2017-01-01-preview"][cnn];
	body = ExportString[<|"query"->query|>,"RawJSON"];
	req = HTTPRequest[url, <| Method->"POST", "Body"->body, "ContentType"->"application/json", "Headers"->{"Authorization"->"Bearer " <> tokenValue@token} |>];
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


azLogAnalyticsKubeContainerShortNames[cnn_] :=
	azLogAnalyticsQuery[cnn, "KubePodInventory | distinct ContainerName "] /. ds_Dataset :> 
			Sort@DeleteDuplicates[Last@StringSplit[#,"/"]& /@ (Normal@ ds["Table_0",All,"ContainerName"] /. "" -> Nothing)];


azLogAnalyticsKubeContainerIds[cnn_, containerNameFilter_String] := 
	azLogAnalyticsQuery[cnn, StringTemplate["KubePodInventory | where ContainerName has '``' | distinct ContainerID"][containerNameFilter]] /.
		ds_Dataset :> (Normal[ds["Table_0",All,"ContainerID"]] /. "" -> Nothing)


azLogAnalyticsContainerLogStatistics[cnn_, containerId_] := 
	azLogAnalyticsQuery[lawPocDefault,StringTemplate[
		"ContainerLog | where ContainerID == '`1`' | summarize ContainerId='`1`',StartLogTime=min(TimeGenerated),EndLogTime=max(TimeGenerated),Count=count()"
	][containerId]] /. 
		ds_Dataset :> Normal[ds["Table_0",SortBy["StartLogTime"]]] /.{v_}:>v


azLogAnalyticsContainerLog[cnn_, containerId_String] := azLogAnalyticsQuery[cnn, StringTemplate[
	"ContainerLog | where ContainerID == '``' | project TimeGenerated,LogEntry "][containerId]] /.
		ds_Dataset:>ds["Table_0", SortBy["TimeGenerated"], <|#,"ContainerId" -> containerId|> &]


End[];


EndPackage[]
