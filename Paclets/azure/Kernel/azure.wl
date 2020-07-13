(* ::Package:: *)

BeginPackage["azure`"];


$azExe;
azLogin;
azGetToken;
azLogAnalyticsQuery;
azGetSubscriptions;


Begin["`Private`"];


(* ::Subsection:: *)
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
			ImportByteArray[StringToByteArray@std,"RawJSON"]
			


(* ::Subsection:: *)
(*Log analytics*)


(* ::Text:: *)
(*https://dev.loganalytics.io/documentation/Overview*)
(*https://docs.microsoft.com/en-us/azure/application-gateway/application-gateway-diagnostics*)


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
	req = HTTPRequest[url, <| Method->"POST", "Body"->body, "ContentType"->"application/json", "Headers"->{"Authorization"->"Bearer " <>token} |>];
	res = URLRead@req;
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


End[];


EndPackage[]
