(* ::Package:: *)

(* ::Title:: *)
(*Azure*)


(* ::Section::Closed:: *)
(*Notes*)


(* ::Text:: *)
(*To do:*)
(*- rename 	list to resource*)
(*- https://stackoverflow.com/questions/58598532/what-is-api-equivalent-of-az-account-list*)


(* ::Section:: *)
(*Header*)


BeginPackage["azure`"];


(* Base *)
azConnections;
azRefresh;
azHttpGet;
azHttpGetPaged;
azHttpGetOPaged;
azHttpPost;
azHttpPut;
azHttpPatch;
azHttpDelete;
azShellGetToken;
azShellGetGraphToken;
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

(* Geo locations*)
azGeoLocationList;
azGeoLocations;

(* ARM Templates *)
azDeploymentTemplateExport;
azDeploymentTemplateCreate;

(* Resource Management *)
azResourceGroups;
azResourceGroupList;
azResourceGroupSearch;
azResources;
azResourceList;
azResourceGroupDeployments;
azResourceGroupDeploymentList;
azResourceGroupDeploymentSearch;
azResourceGroupDeploymentValidate;
azResourceGroupDeploymentWhatIf;
azResourceGroupDeploymentCreate;

(* MS Graph *)
azGraphUsers;
azGraphUserList;
azGraphGroups;
azGraphGroupList;

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
azLogQueryKubeLogSummary;
azLogQueryKubeContainerEntriesAround;
azLogAnalyticsKubeContainerShortNames;
azLogAnalyticsKubeContainerIdToShortName;
azLogAnalyticsKubeContainerIds;
azLogAnalyticsKubeContainerLogStatistics;
azLogAnalyticsKubeContainerLog;
azLogAnalyticsKubeSearchContainerLogs;

(* API manager *)
azApimPolicy;
azApiManagementServices;
azApiManagementServiceList;
azApiManagementServiceSearch;
azApiManagementServicePolicies;
azApiManagementServicePolicyList;
azApiManagementServicePolicySearch;
azApiManagementLoggers;
azApiManagementLoggerList;
azApiManagementLoggerSearch;
azApiManagementSubscriptions;
azApiManagementSubscriptionList;
azApiManagementSubscriptionSearch;
azApiManagementProducts;
azApiManagementProductList;
azApiManagementProductSearch;
azApiManagementProductPolicies;
azApiManagementProductPolicyList;
azApiManagementProductPolicySearch;
azApiManagementApis;
azApiManagementApiList;
azApiManagementApiSearch;
azApiManagementApiRevisions;
azApiManagementApiRevisionList;
azApiManagementApiSchemas;
azApiManagementApiSchemaList;
azApiManagementApiSchemaSearch;
azApiManagementApiPolicies;
azApiManagementApiPolicyList;
azApiManagementApiPolicySearch;
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
azApiManagementApiOperationPolicies;
azApiManagementApiOperationPolicyList;
azApiManagementApiOperationPolicySearch;
azApiManagementApiReleases;
azApiManagementApiReleaseList;
azApiManagementApiReleaseSearch;
azApiManagementApiVersionSets;
azApiManagementApiVersionSetList;
azApiManagementApiVersionSetSearch;
azApiManagementApiDiagnostics;
azApiManagementApiDiagnosticList;
azApiManagementApiDiagnosticSearch;

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
azAppGatewayAvailableWafRuleList;

(* VirtualNetwork *)
azVirtualNetworks;
azVirtualNetworkList;
azVirtualNetworkSubnets;
azVirtualNetworkSubnetList;
azVirtualNetworkSubnetSearch;
azVirtualNetworkNics;
azVirtualNetworkNicList;
azVirtualNetworkNicSearch;
azVirtualNetworkNicRouteTableList;
azVirtualNetworkPublicIps;
azVirtualNetworkPublicIpList;
azVirtualNetworkPublicIpSearch;
azVirtualNetworkSecurityGroups;
azVirtualNetworkSecurityGroupList;
azVirtualNetworkSecurityGroupSearch;
azVirtualNetworkRouteTables;
azVirtualNetworkRouteTableList;
azVirtualNetworkRouteTableSearch;

(* Key Vaults *)
azKeyVaults;
azKeyVaultList;
azKeyVaultSearch;
azKeyVaultKeyList;
azKeyVaultSecretList;

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

(* Azure DevOps - Task Agent *)
azDevOpsAgentTaskGroups;
azDevOpsAgentTaskGroupList;
azDevOpsAgentTaskGroupSearch;
azDevOpsAgentPools;
azDevOpsAgentPoolList;
azDevOpsAgentPoolSearch;
azDevOpsAgents;
azDevOpsAgentList;
azDevOpsAgentSearch;
azDevOpsAgentEnvironments;
azDevOpsAgentEnvironmentList;
azDevOpsAgentEnvironmentSearch;
azDevOpsAgentVariableGroups;
azDevOpsAgentVariableGroupList;
azDevOpsAgentVariableGroupSearch;
azDevOpsAgentClouds;
azDevOpsAgentCloudList;
azDevOpsAgentCloudSearch;
azDevOpsAgentCloudTypes;
azDevOpsAgentDeploymentGroups;
azDevOpsAgentDeploymentGroupList;
azDevOpsAgentDeploymentGroupSearch;
azDevOpsAgentQueues;
azDevOpsAgentQueueList;
azDevOpsAgentQueueSearch;

(* Service endpoints/connections *)
azDevOpsServiceEndpoints;
azDevOpsServiceEndpointList;
azDevOpsServiceEndpointSearch;
azDevOpsServiceEndpointTypes;
azDevOpsServiceEndpointExecutionHistory;

(* Extensions *)
azDevOpsInstalledExtensions;
azDevOpsInstalledExtensionList;
azDevOpsInstalledExtensionSearch;



(* temp *)
azMicrosoftIdToAzRef;
refToAzureId;
parseUri;
refType;
azMsTypeFromId;


Begin["`Private`"];


(* ::Section:: *)
(*Base*)


(* ::Subsection::Closed:: *)
(*Core*)


apiVersionFromUrl[url_String] :=
	StringCases[url,"api-version="~~ver:(Except["&"]..):>ver,IgnoreCase->False]/.{v_}:>v;


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

parseUri[txt_String, strPattern_String] := parseUri[
	txt,
	StringSplit[strPattern, "`" ~~ key : (Except["`"] ..) ~~ "`" :> Key@key],
	<||>
];
parseUri[txt_String, {first_String, rest___}, result_Association] := If[
	StringMatchQ[txt, StartOfString ~~ first ~~ ___, IgnoreCase -> True],
	parseUri[StringReplace[txt, StartOfString ~~ first :> "", IgnoreCase->True], {rest}, result],
	$Failed
];
parseUri[txt_String, {Key[key_String], rest___}, result_Association] :=
	parseUri[
		StringReplace[txt, (StartOfString ~~ Except["/"] ..) -> ""],
		{rest},
		Append[result, key -> StringCases[txt, (StartOfString ~~ Except["/"] ..)] /. {v_} :> v]
	];
parseUri[_, {}, result_Association] := result;


relations={};
azRelations[]:=relations;


azRef/:Normal[azRef[a_Association]] := a;
azRef[a_Association][property_String] := a[property];
azRef[a_Association]["subscriptionRef"] := azRef[<|
	"azType" -> "azure.subscription",
	If[KeyExistsQ[a,"subscriptionName"],"subscriptionName" -> a["subscriptionName"], Nothing],
	"subscriptionId" -> a["subscriptionId"]
|>];
azRef[a_Association]["resourceGroupRef"] := azRef[<|
	"azType" -> "azure.resourceGroup",
	If[KeyExistsQ[a,"subscriptionName"],"subscriptionName" -> a["subscriptionName"], Nothing],
	"subscriptionId" -> a["subscriptionId"],
	"resourceGroupName" -> a["resourceGroupName"]
|>];


azRef /: MissingQ[azRef[data_]] := MissingQ@data["azType"];

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
	res /. r:HTTPResponse[_,KeyValuePattern[{"StatusCode"->(200|201)}],___] :> 
		Dataset@ImportByteArray[r["BodyByteArray"],"RawJSON"]


(* https://docs.microsoft.com/en-us/previous-versions/azure/dn645543(v=azure.100)?redirectedfrom=MSDN *)
azShellGetGraphToken[] := 
	azShellGetToken@{"account", "get-access-token", "--resource-type", "ms-graph"};
azShellGetToken[azRef[KeyValuePattern["subscriptionId" -> id_String]], args___] := azShellGetToken[id, args];
azShellGetToken[subscriptionId_String] :=
	azShellGetToken@{"account","get-access-token","--subscription", subscriptionId};
azShellGetToken[{args___}] := RunProcess[{$azExe, args}] /.
	KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_,"--subscription" ->subscriptionId }] :> 
		ImportString[std,"RawJSON"] /. KeyValuePattern[{"ExitCode"->0,"StandardOutput"->std_}] :> 
			With[
				{token = ImportByteArray[StringToByteArray@std,"RawJSON"]}, 
				Append[token, "authorizationHeader" -> "Bearer "<> token["accessToken"]]
			];

toIso8601[{from_,to_}] := toIso8601@from<>"/"<>toIso8601@to;
toIso8601[date_DateObject] := DateString[DateObject[date,TimeZone->"Zulu"],"ISODateTime"]<>"Z";

azHttpGetPaged[auth_ ,args___] := azHttpGet[auth, args] // 
	(pageData[auth, #] /. l_List :> Dataset@l) & 

azHttpGetOPaged[auth_ ,args___] := azHttpGet[auth, args] // 
	(pageOData[auth, #] /. l_List :> Dataset@l) & 

azHttpGet[auth_,{urlTemplate_String,azRef[data_Association]}, args___]:=
	azHttpGet[auth,StringTemplate[urlTemplate][URLEncode/@ data],args];
azHttpGet[authorizationHeader_String,  url_String] := Module[
	{req, res},
	req = HTTPRequest[url, <|
		Method -> "GET",
		"ContentType" -> "application/json",
		"Headers" -> {
			"Authorization" -> authorizationHeader,
			"Accept" -> "application/json"
		}
	|>];
	Sow[req];
	res = URLRead[req, Interactive -> False];
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
	res = URLRead[req, Interactive -> False];
	Sow[res];
	res
]

azHttpPost[auth_, ref_] := azHttpPost[auth,ref, <||>];
azHttpPost[auth_,{urlTemplate_String,azRef[refData_Association]}, args___]:=
	azHttpPost[auth,{urlTemplate,URLEncode/@ refData},args];
azHttpPost[auth_,{urlTemplate_String, data_Association}, args___]:=
	azHttpPost[auth,StringTemplate[urlTemplate][URLEncode/@ data],args];	
azHttpPost[authorizationHeader_String,  url_String, data_Association] :=	
	azHttpPost[authorizationHeader,  url, ExportString[data,"RawJSON"]];
azHttpPost[authorizationHeader_String,  url_String, body_String] := Module[
	{req, res},
	req = HTTPRequest[url, <|
		Method -> "POST",
		"ContentType" -> "application/json",
		"Headers" -> {"Authorization" -> authorizationHeader},
		"Body" -> body
	|>];
	Sow[req];
	res = URLRead[req, Interactive -> False];
	Sow[res];
	azParseRestResponde@res
]

azHttpPut[auth_, ref_] := azHttpPut[auth,ref, <||>];
azHttpPut[auth_,{urlTemplate_String,azRef[refData_Association]}, args___]:=
	azHttpPut[auth,{urlTemplate,URLEncode/@ refData},args];
azHttpPut[auth_,{urlTemplate_String, data_Association}, args___]:=
	azHttpPut[auth,StringTemplate[urlTemplate][URLEncode/@ data],args];	
azHttpPut[authorizationHeader_String,  url_String, data_Association] :=	
	azHttpPut[authorizationHeader,  url, ExportString[data,"RawJSON"]];
azHttpPut[authorizationHeader_String,  url_String, body_String] := Module[
	{req, res},
	req = HTTPRequest[url, <|
		Method -> "PUT",
		"ContentType" -> "application/json",
		"Headers" -> {"Authorization" -> authorizationHeader},
		"Body" -> body
	|>];
	Sow[req];
	res = URLRead[req, Interactive -> False];
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
	res = URLRead[req, Interactive -> False];
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
	res = URLRead[req, Interactive -> False];
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
	res = URLRead[req, Interactive -> False];
	Sow[res];
	ImportByteArray[res["BodyByteArray"],"ZIP"]
]


azInfo[auth_, msId_String] := azInfo[auth, {ToLowerCase@azMsTypeFromId@msId ,msId}];


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
		
pageData[auth_, r_HTTPResponse] := r;
pageData[auth_, ds_Dataset] := pageData[auth, Normal@ds];
pageData[auth_,data_Association] := (
	PrintTemporary[StringTemplate["Paging: ``"][Length@data["value"]]];
	data["value"] ~ Join ~ pageData[auth, azHttpGet[auth,data["nextLink"]]]) /;
				KeyExistsQ[data,"nextLink"];
pageData[auth_String,data_Association]:=data["value"] /;
				!KeyExistsQ[data,"nextLink"];

pageOData[auth_, r_HTTPResponse] := r;
pageOData[auth_, ds_Dataset] := pageOData[auth, Normal@ds];
pageOData[auth_,data_Association] := (
	PrintTemporary[StringTemplate["Paging: ``"][Length@data["value"]]];
	data["value"] ~ Join ~ pageOData[auth, azHttpGet[auth,data["@odata.nextLink"]]]) /;
				KeyExistsQ[data,"@odata.nextLink"];
pageOData[auth_String,data_Association]:=data["value"] /;
				!KeyExistsQ[data,"@odata.nextLink"];
																								
resolveHttpStatusCode202[auth_, response_HTTPResponse] := Module[
	{res=response, loc},
	While[
		MatchQ[res,_HTTPResponse] && res["StatusCode"] == 202,
		loc = Association[res["Headers"]]["location"];
		res = azHttpGet[auth, loc];
		PrintTemporary["Waiting for response 202 to be resolved..."];
		Pause[2];
	];
	res
];
resolveHttpStatusCode202[_, v_] := v;


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
	Arrowheads[_]-> Arrowheads[0.005]
]


azConnections[l:{azRef[KeyValuePattern["azType"->"azure.subscription"]]...}] := 
	azConnections[<| #["subscriptionId"] -> <| "subscriptionName" -> #["subscriptionName"], azShellGetToken[#] |> & /@ l |>];
azConnections[data_Association][subscriptionId_String] :=
	data[subscriptionId,"authorizationHeader"];
azConnections[data_Association][ref_azRef] := 
	data[ref["subscriptionId"]];
azConnections[data_Association]["Names"] := 
	Dataset[data][All,"subscriptionName"] // Values // Normal;
azConnections[data_Association][{subName_String}] := 
	(Dataset[data][Select[#["subscriptionName"]== subName &],"authorizationHeader"] // Values // Normal )/. {v_} :> v;
azConnections /: f_[cnns:azConnections[_Association],refs_List,args___] := 
	Dataset@Association[(# -> f[cnns[#["subscriptionId"]],#,args] & /@ refs) /. ds_Dataset :> Normal@ds]
 
azConnections /: f_[cnns:azConnections[_Association],ref_azRef,args___] :=	
	f[cnns,{ref},args]


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

azureInfoIdBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"getUrl"->_String,
	"microsoftType" -> _String
}]] := TemplateObject[Hold[
azInfo[authorizationHeader_String, {TemplateExpression[ToLowerCase@TemplateSlot["microsoftType"]], id_String}] := Module[
		{apiVersion, url},
		apiVersion = apiVersionFromUrl[TemplateSlot["getUrl"]];
		url = "https://management.azure.com" <> id <> "?api-version=" <> apiVersion;
		azHttpGet[authorizationHeader, url] /. ds_Dataset :> ds[<|
			"azRef" -> azRef[<|
				"azType" -> TemplateSlot["azType"],
				TemplateSlot["listResultKeysFunc"][#]
			|>],
			# 
		|> &]
	]
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
azParent[auth_, azRefAzurePattern[TemplateSlot["azType"]]] :=
	azParent[auth, ref, TemplateSlot["parentAzType"]];
AppendTo[relations, {TemplateSlot["azType"]->TemplateSlot["parentAzType"], {"azParent"}}];
]][cfg] // ReleaseHold

azureMicrosoftIdBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"microsoftType"->_String,
	"microsoftIdTemplate"->_String
}]] := TemplateObject[Hold[
microsoftIdInfo[TemplateSlot["microsoftType"]] :=
<|
	"azType" ->  TemplateSlot["azType"],
	"idTemplate" -> TemplateSlot["microsoftIdTemplate"]
|>
]][cfg] // ReleaseHold

azureDeleteBuilder[cfg:KeyValuePattern[{
	"azType"->_String,
	"getUrl"->_String
}]] := TemplateObject[Hold[
azDelete[authorizationHeader_String, azRefAzurePattern[TemplateSlot["azType"]]] :=
	azHttpDelete[authorizationHeader,
		StringTemplate[TemplateSlot["getUrl"]][URLEncode/@refData]
	] 
]][cfg] // ReleaseHold


azureDefaultOperationsBuilder[cfg_Association] := Module[
	{res = {}},
	azurePanelInfoBuilder[cfg] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "uiUrl"],
		azureOpenUiBuilder[cfg],
		Null] // AppendTo[res,#] &;
	azureInfoBuilder[cfg] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "microsoftType"],
		azureInfoIdBuilder[cfg],
		Null] // AppendTo[res,#] &; 
	azureListBuilder[cfg] // AppendTo[res,#] &;
	azureRestDocumentationBuilder[cfg] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "searchFields"],
		azureSearchBuilder[cfg],
		Null] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "parentAzType"],
		azureRelationBuilder[cfg]; azureParentBuilder[cfg],
		Null] // AppendTo[res,#] &;
	If[KeyExistsQ[cfg, "microsoftIdTemplate"],
		azureMicrosoftIdBuilder[cfg],
		Null] // AppendTo[res,#] &;
	azureDeleteBuilder[cfg] // AppendTo[res,#] &;				
	res
];



hasUpperQ[s_String]:= !LowerCaseQ[StringJoin@StringCases[s,LetterCharacter]];

microsoftIdInfo[msType_String] := microsoftIdInfo@ToLowerCase@msType /; hasUpperQ[msType];
microsoftIdInfo[msType_] := <|
	"idTemplate" -> "",
	"azType" -> Missing[msType]
|>

azMsTypeFromId[msId_String] :=
	StringCases[
		msId, "/providers" ~~ types:(("/"~~(Except["/"]..))..) :> types, IgnoreCase->True] /.
			{v_} :> StringSplit[v,"/"] /. 
			{provider_,rest___} :> StringRiffle[{provider}~Join~{rest}[[;;;;2]],"/"] 

azMicrosoftIdToAzRef[msId_String]:=With[
	{idInf=microsoftIdInfo@azMsTypeFromId@msId},
	azRef[<|
		"azType" -> idInf["azType"],
		parseUri[msId, idInf["idTemplate"]]
	|> ]
]


(* ::Subsection::Closed:: *)
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
azParent[auth_, azRefDevOpsPattern[TemplateSlot["azType"]]] :=
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
icons =  Image[ImageResize[#,15],ImageSize -> 15]& /@ azIcons;
panelInfo[_] := <|
	"icon"->icons["azure.default"],
	"labelFunc"->("-"&)
|>

refType[KeyValuePattern["azType"->type_String]] := (Last@StringSplit[type,"."] // (Last@StringSplit[#,"/"] &)) /; 
	StringContainsQ[type,"."];
refType[KeyValuePattern["azType" -> Missing[t_String]]] := t;
refType[_] := "unkown type";

azRef /: MakeBoxes[ref:azRef[refData_Association], fmt_] :=
Block[{type, typeLabel, bg, display, panelInf},
	type = refData["azType"] /. Missing[t_]:>t;
	typeLabel = refType[refData];
	panelInf = panelInfo[refData["azType"]];
	bg = Which[
		StringContainsQ[type, "azure."], Lighter[LightBlue],
		StringContainsQ[type ,"devOps."], Lighter[LightGreen],
		True, Lighter[Lighter[LightRed]]
	];
	display = Tooltip[
		Framed[
			Row[{panelInf["icon"]," ",Style[typeLabel, Italic], ": ", Style[panelInf["labelFunc"][refData],Bold]}],
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


(* ::Subsection:: *)
(*Subscription*)


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
(*Graph*)


(* ::Subsection:: *)
(*Users*)


azGraphUsers[auth_] :=
	azGraphUserList[auth] /. ds_Dataset :> Normal@ds[All, "azRef"];
azGraphUserList[auth_] := 
	azHttpGetOPaged[auth, "https://graph.microsoft.com/v1.0/users"] /. ds_Dataset :> ds[All,
		<| "azRef" -> azRef[<|
			"azType" -> "azure.graph.user",
			"name" -> #["displayName"],
			"email" -> #["mail"],
			"userId" -> #["id"]
		|>], # |> &
	]
	
panelInfo["azure.graph.user"] := <|
	"icon" -> icons["azure.graph.user"],
	"labelFunc" -> Function[{refData}, refData["name"]]
|>;
azIcon[azRefAzurePattern["azure.graph.user"]] := azIcon[refData["azType"]];
azIcon["azure.graph.user"] := icons["azure.graph.user"];



(* ::Subsection:: *)
(*Groups*)


azGraphGroups[auth_] :=
	azGraphGroupList[auth] /. ds_Dataset :> Normal@ds[All, "azRef"];
azGraphGroupList[auth_] := 
	azHttpGetOPaged[auth, "https://graph.microsoft.com/v1.0/groups"] /. ds_Dataset :> ds[All,
		<| "azRef" -> azRef[<|
			"azType" -> "azure.graph.group",
			"name" -> #["displayName"],
			"groupId" -> #["id"]
		|>], # |> &
	]
	
panelInfo["azure.graph.group"] := <|
	"icon" -> icons["azure.graph.group"],
	"labelFunc" -> Function[{refData}, refData["name"]]
|>;
azIcon[azRefAzurePattern["azure.graph.user"]] := azIcon[refData["azType"]];
azIcon["azure.graph.group"] := icons["azure.graph.group"];



(* ::Section::Closed:: *)
(*Resource management*)


(* ::Subsection:: *)
(*Resource groups*)


cfg = <|
	"azType"->"azure.resourceGroup",
	"nameSingular"->"ResourceGroup",
	"namePlural"->"ResourceGroups",
	"panelIcon"-> icons["azure.resourceGroup"],
	"panelLabelFunc"-> Function[{refData}, refData["resourceGroupName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/resources/resourcegroups",
	"uiUrl" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/overview",
	"getUrl"-> "https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`?api-version=2020-06-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups?api-version=2020-06-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


azResourceGroupList[auth_, ref_azRef] := 
	azRef[<|
		"azType" -> "azure.resourceGroup",
		"subscriptionId" -> ref["subscriptionId"],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> ref["resourceGroupName"]
	|>] /; ref["azType"]=!="azure.subscription" 


(* ::Subsection::Closed:: *)
(*Export Deployment Template*)


azDeploymentTemplateExport[auth_, azRefAzurePattern["azure.resourceGroup"]] := azDeploymentTemplateExport[auth, ref, "*"] 
azDeploymentTemplateExport[auth_, azRefAzurePattern["azure.resourceGroup"], resourcePattern_String] := Module[{data},
	data = <|
		"resources"->{resourcePattern},
		"options"->"IncludeParameterDefaultValue,IncludeComments"
	|>;
	azHttpPost[auth, {
			"https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/exportTemplate?api-version=2020-06-01",
			ref
		}, ExportString[data,"RawJSON"]
	] // resolveHttpStatusCode202[auth, #]&
];
azDeploymentTemplateExport[auth_, ref_] := 
	(azInfo[auth, ref]["id"]) /. id_String :> azDeploymentTemplateExport[auth, ref["resourceGroupRef"], id]


(* ::Subsection::Closed:: *)
(*Create Deployment Template*)


azDeploymentTemplateCreate[data:KeyValuePattern[{
	"parameters" -> _Association,
	"variables" -> _Association,
	"resources" -> _List
}]] :=
	TemplateObject[
		<|
			"$schema" -> "http://schema.management.azure.com/schemas/2015-01-01/deploymentTemplate.json#",
			"contentVersion" -> "1.0.0.0",
			"parameters" -> TemplateSlot["parameters"],
			"variables" -> TemplateSlot["variables"],
			"resources" -> TemplateSlot["resources"]
		|>][data];


(* ::Subsection::Closed:: *)
(*Resources*)


azResources[auth_, ref_] := azResourceList[auth, ref] /. ds_Dataset :> Normal@ds[All,"azRef"]

azResourceList[auth_, azRefAzurePattern["azure.subscription"]] := 
	azHttpGet[
		auth, {
			"https://management.azure.com/subscriptions/`subscriptionId`/resources?api-version=2020-06-01",
			ref
		}
	] /. ds_Dataset :> ds["value", All, <| "azRef" -> azMicrosoftIdToAzRef[#id], # |>&]
	
azResourceList[auth_, azRefAzurePattern["azure.resourceGroup"]] := 
	azHttpGet[
		auth, {
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/resources?api-version=2020-06-01",
			ref
		}
	] /. ds_Dataset :> ds["value",All, <| "azRef" ->  azMicrosoftIdToAzRef[#id], # |>&];


(* ::Subsection:: *)
(*Resource group deployments*)


(* ::Subsubsection:: *)
(*Deployment*)


cfg = <|
	"azType"->"azure.resourceGroupDeployment",
	"nameSingular"->"ResourceGroupDeployment",
	"namePlural"->"ResourceGroupDeployments",
	"panelIcon"-> icons["azure.resource.deployment"],
	"panelLabelFunc"-> Function[{refData}, refData["deploymentName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/resources/deployments",
	"getUrl"-> "https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.Resources/deployments/`deploymentName`?api-version=2020-06-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.Resources/deployments/?api-version=2020-06-01",
	"listFilter" -> azRefAzurePattern["azure.resourceGroup"],
	"parentAzType" -> "azure.resourceGroup",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"deploymentName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Subsubsection::Closed:: *)
(*Validate  deployment*)


azResourceGroupDeploymentValidate[
	auth_,
	azRefAzurePattern["azure.resourceGroup"],
	deploymentName_String,
	template_Association,
	templateParamters_Association
] :=
	azHttpPost[auth,
		{
			"https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.Resources/deployments/`deploymentName`/validate?api-version=2020-06-01",
			<| refData, "deploymentName" -> deploymentName |>
		},
		<|
			"properties" -> <|
				"mode" -> "Incremental",
				"parameters" -> templateParamters,
				"template" -> template	
			|>,
			"tags" -> <||>
		|>
	]


(* ::Subsection::Closed:: *)
(*What if deployment*)


azResourceGroupDeploymentWhatIf[
	auth_,
	azRefAzurePattern["azure.resourceGroup"],
	deploymentName_String,
	template_Association,
	templateParamters_Association
] :=
	azHttpPost[auth,
		{
			"https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.Resources/deployments/`deploymentName`/whatIf?api-version=2020-06-01",
			<| refData, "deploymentName" -> deploymentName |>
		},
		<|
			"properties" -> <|
				"mode" -> "Incremental",
				"parameters" -> templateParamters,
				"template" -> template	
			|>
		|>
	]  // resolveHttpStatusCode202[auth, #]&


(* ::Subsection::Closed:: *)
(*Create deployment*)


azResourceGroupDeploymentCreate[
	auth_,
	azRefAzurePattern["azure.resourceGroup"],
	deploymentName_String,
	template_Association,
	templateParamters_Association
] :=
	azHttpPut[auth,
		{
			"https://management.azure.com/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.Resources/deployments/`deploymentName`?api-version=2020-06-01",
			<| refData, "deploymentName" -> deploymentName |>
		},
		<|
			"properties" -> <|
				"mode" -> "Incremental",
				"parameters" -> templateParamters,
				"template" -> template	
			|>
		|>
	]  // resolveHttpStatusCode202[auth, #]&


(* ::Subsection::Closed:: *)
(*Geo Locations*)


azGeoLocationList[auth_, azRefAzurePattern["azure.subscription"]] :=
	azHttpGet[auth, {
		"https://management.azure.com/subscriptions/`subscriptionId`/locations?api-version=2020-01-01",
		ref
	}] /. ds_Dataset :> ds["value"];
	
azGeoLocations[auth_, ref_] := azGeoLocationList[auth, ref] /. ds_Dataset :> Normal@ds[All, "name"];


(* ::Section:: *)
(*Azure kubernetes service*)


cfg = <|
	"azType"->"azure.aks.cluster",
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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"clusterName" -> res["name"]
	|>],
	"searchFields" -> {"name"},
	"microsoftType" -> "Microsoft.ContainerService/managedClusters",
	"microsoftIdTemplate" -> "/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.ContainerService/managedClusters/`clusterName`"
|>;

azureDefaultOperationsBuilder[cfg]


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
	"searchFields" -> {"name"},
	"microsoftType" -> "Microsoft.OperationalInsights/workspaces",
	"microsoftIdTemplate" -> "/subscriptions/`subscriptionId`/resourcegroups/`resourceGroupName`/providers/Microsoft.OperationalInsights/workspaces/`workspaceName`"
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




(* ::Subsection:: *)
(*AKS*)


(* ::Text:: *)
(*https://docs.microsoft.com/en-us/azure/azure-monitor/insights/containers*)


azLogQueryKubeLogSummary[auth_, ref_] :=
	azLogQuery[auth, ref, "
ContainerLog |
summarize Count=count(), StartDate=min(TimeGenerated), EndDate=max(TimeGenerated) by ContainerID |
join (KubePodInventory | distinct ContainerID, ContainerName ) 
on $left.ContainerID == $right.ContainerID"
]  /. ds_Dataset :> 
	ds[1, All, <|
		"ContainerBaseName" -> Last@StringSplit[#["ContainerName"], "/"],
		"ContainerName" -> #["ContainerName"], 
		"ContainerId" -> #["ContainerID"],
		"LineCount" -> #["Count"],
		"StartDate" -> DateObject[#["StartDate"],"Instant",TimeZone->"Zulu"],
		"EndDate"->DateObject[#["EndDate"],"Instant",TimeZone->"Zulu"]
	|>&][SortBy["ContainerBaseName"] 
] 


azLogQueryKubeContainerEntriesAround[cnn_, law_ , logEntry_Association] :=
	azLogQueryKubeContainerEntriesAround[cnn, law, logEntry, 2];
azLogQueryKubeContainerEntriesAround[cnn_, law_ , logEntry_Association,seconds_Integer] := Module[
	{entryTime, containerId},
	entryTime=logEntry["TimeGenerated"];
	containerId=logEntry["ContainerID"];
	azLogQuery[
		cnn, 
		law, 
		StringTemplate["ContainerLog | where ContainerID == \"``\"  "][containerId],
		{entryTime-Quantity[seconds,"Seconds"], entryTime+Quantity[seconds,"Seconds"]}
	] /. ds_Dataset :> ds["Table_0"]
];


azLogAnalyticsKubeContainerShortNames[auth_,ref_, args___] :=
	azLogQuery[auth, ref, "KubePodInventory | distinct ContainerName ", args] /. ds_Dataset :> 
			Sort@DeleteDuplicates[Last@StringSplit[#,"/"]& /@ (Normal@ ds["Table_0",All,"ContainerName"] /. "" -> Nothing)];


azLogAnalyticsKubeContainerIds[auth_,ref_, containerNameFilter_String] := 
	azLogQuery[auth, ref, StringTemplate["KubePodInventory | where ContainerName has '``' | distinct ContainerID"][containerNameFilter]] /.
		ds_Dataset :> (Normal[ds["Table_0",All,"ContainerID"]] /. "" -> Nothing);
		
azLogAnalyticsKubeContainerIds[auth_,ref_] := 
	azLogQuery[auth, ref, StringTemplate["KubePodInventory  | distinct ContainerID"][containerNameFilter]] /.
		ds_Dataset :> (Normal[ds["Table_0",All,"ContainerID"]] /. "" -> Nothing)


azLogAnalyticsKubeContainerLogStatistics[auth_,ref_ , containerId_] := 
	azLogQuery[auth, ref, StringTemplate[
		"ContainerLog | where ContainerID == '`1`' | summarize ContainerId='`1`',StartLogTime=min(TimeGenerated),EndLogTime=max(TimeGenerated),Count=count()"
	][containerId]] /. 
		ds_Dataset :> Normal[ds["Table_0",SortBy["StartLogTime"]]] /.{v_}:>v


azLogAnalyticsKubeContainerLog[auth_,ref_, containerIds_List, dateRange_] := 
	Dataset[Flatten[Normal@azLogAnalyticsKubeContainerLog[auth,ref,#,dateRange] & /@ containerIds]];

azLogAnalyticsKubeContainerLog[auth_,ref_, containerId_String, dateRange_: Null] := 
	azLogQuery[auth, ref, StringTemplate[
		"ContainerLog | where ContainerID == '``' | project TimeGenerated,LogEntry, ContainerID "][containerId], 
		dateRange] /.
			ds_Dataset :> ds["Table_0", SortBy["TimeGenerated"]];
			
azLogAnalyticsKubeContainerLog[auth_,ref_, All, dateRange_] := 
	azLogQuery[auth, ref, 
		"ContainerLog | project TimeGenerated,LogEntry, ContainerID ", 
		dateRange] /.
			ds_Dataset:>ds["Table_0", SortBy["TimeGenerated"]]


azLogAnalyticsKubeContainerIdToShortName[auth_,ref_, containerId_String] :=
	azLogQuery[auth, ref, 
		StringTemplate["KubePodInventory | where ContainerID == '``' | take 1 "][containerId]] /.
			ds_Dataset :> Last@StringSplit[Normal@ds["Table_0",1,"ContainerName"], "/"]


azLogAnalyticsKubeSearchContainerLogs[auth_,ref_, str_String, dateRange_: Null] := azLogQuery[auth, ref, StringTemplate[
	"ContainerLog | where LogEntry  contains '``' | take 1000 | project TimeGenerated,LogEntry,ContainerID "][str], dateRange] /.
		ds_Dataset:>ds["Table_0"]


(* ::Section::Closed:: *)
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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"gatewayName" -> res["name"]
	|>],
	"searchFields" -> {"name"},
	"microsoftType" -> "Microsoft.Network/applicationGateways",
	"microsoftIdTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/applicationGateways/`gatewayName`"
|>;

azureDefaultOperationsBuilder[cfg]


azAppGatewayAvailableWafRuleList[auth_, azRefAzurePattern["azure.subscription"]] := 
	azHttpGet[
		auth, {
			"https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Network/applicationGatewayAvailableWafRuleSets?api-version=2020-05-01",
			ref
		}
	] /. ds_Dataset :> ds["value"];


(* ::Section::Closed:: *)
(*Monitor*)


(* ::Subsection:: *)
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


(* ::Section::Closed:: *)
(*API manager*)


(* ::Subsection::Closed:: *)
(*Helpers*)


azApimPolicy[auth_,ref:azRef[KeyValuePattern["azType"->"azure.apiManagement.product"]]] :=
	azApiManagementProductPolicyList[auth,ref]/.ds_Dataset :>(Normal[ds]/.{{}->None,{v_}:> v["properties","value"]} );
azApimPolicy[auth_,ref:azRef[KeyValuePattern["azType"->"azure.apiManagement.service"]]] :=
	azApiManagementServicePolicyList[auth,ref]/.ds_Dataset :>(Normal[ds]/.{{}->None,{v_}:> v["properties","value"]} );
azApimPolicy[auth_,ref:azRef[KeyValuePattern["azType"->"azure.apiManagement.api"]]] :=
	azApiManagementApiPolicyList[auth,ref]/.ds_Dataset :>(Normal[ds]/.{{}->None,{v_}:> v["properties","value"]} );
azApimPolicy[auth_,ref:azRef[KeyValuePattern["azType"->"azure.apiManagement.api.operation"]]] :=
	azApiManagementApiOperationPolicyList[auth,ref]/.ds_Dataset :>(Normal[ds]/.{{}->None,{v_}:> v["properties","value"]} );


(* ::Subsection:: *)
(*Services*)


(* ::Subsubsection:: *)
(*Service*)


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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Subsubsection::Closed:: *)
(*Service (global) policy*)


cfg = <|
	"azType"->"azure.apiManagement.service.policy",
	"nameSingular"-> "ApiManagementServicePolicy",
	"namePlural"-> "ApiManagementServicePolicies",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData},  refData["policyName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/policy",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/dev-portal-poc/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/policies/policy?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/policies?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.service"],
	"parentAzType" -> "azure.apiManagement.service",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"policyName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"loggerName" -> res["name"]
	|>],
	"searchFields" -> {"name"},
	"microsoftType" -> "Microsoft.ApiManagement/service/loggers"
|>;
azureDefaultOperationsBuilder[cfg]


azApiManagementLoggerList[auth_, azRefAzurePattern["azure.apiManagement.api.diagnostic"]] := Module[
	{diag},
	diag = azInfo[auth, ref];
	{Normal@azInfo[auth, diag["properties","loggerId"]]} // Dataset
];

AppendTo[relations, {"azure.apiManagement.api.diagnostic"->"azure.apiManagement.logger", {"azApiManagementLoggers","azApiManagementLoggerList"}}];


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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"subscriptionName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;


azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
(*Products*)


(* ::Subsubsection::Closed:: *)
(*Product*)


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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"productName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"},
	"microsoftType" -> "Microsoft.ApiManagement/service/apis/products",
	"microsoftIdTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/products/`productName`"
|>;


azureDefaultOperationsBuilder[cfg]


(* ::Subsubsection::Closed:: *)
(*Product policy*)


cfg = <|
	"azType"->"azure.apiManagement.product.policy",
	"nameSingular"-> "ApiManagementProductPolicy",
	"namePlural"-> "ApiManagementProductPolicies",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["productName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/productpolicy",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/products/`productName`/productPolicies",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/products/`productName`/policies/policy?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/products/`productName`/policies?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.product"],
	"parentAzType" -> "azure.apiManagement.product",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"productName" -> getIdKeyValue[res["id"],"products/"],
		"policyName" -> res["name"]	
	|>],
	"searchFields" -> {"name"}
|>;


azureDefaultOperationsBuilder[cfg];


(* ::Subsubsection::Closed:: *)
(*List API's*)


azApiManagementApiList[auth_, azRefAzurePattern["azure.apiManagement.product"]] :=
	azHttpGet[auth,{
		"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/products/`productName`/apis?api-version=2019-12-01",
		ref
	}] /. ds_Dataset :> ds[
		"value",
		All,
		<| 
			"azRef"-> azRefresh[auth, azMicrosoftIdToAzRef[
				StringReplace[#["id"],"/products/"~~(Except["/"]..)->""]
			]],
			#
		|> &];

AppendTo[relations, {"azure.apiManagement.product"->"azure.apiManagement.api", {"azApiManagementApis","azApiManagementApiList"}}];


(* ::Subsection:: *)
(*API's*)


(* ::Subsubsection::Closed:: *)
(*API*)


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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> res["name"],
		"apiDisplayName" -> res["properties","displayName"],
		"apiRevision" -> res["properties","apiRevision"] 
		
	|>],
	"searchFields" -> {"name"},
	"microsoftType" -> "Microsoft.ApiManagement/service/apis",
	"microsoftIdTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`"
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsubsection::Closed:: *)
(*API policy*)


cfg = <|
	"azType"->"azure.apiManagement.api.policy",
	"nameSingular"-> "ApiManagementApiPolicy",
	"namePlural"-> "ApiManagementApiPolicies",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["apiName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apipolicy",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/policies/policy?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/policies?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.api"],
	"parentAzType" -> "azure.apiManagement.api",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> getIdKeyValue[res["id"],"apis/"],
		"policyName" -> res["name"] 

	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg];


(* ::Subsubsection::Closed:: *)
(*List products's*)


azApiManagementProductList[auth_, azRefAzurePattern["azure.apiManagement.api"]] :=
	azHttpGet[auth,{
		"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/products?api-version=2019-12-01",
		ref
	}]  /. ds_Dataset :> ds["value", All ,<|"azRef"-> azRefresh[auth, azMicrosoftIdToAzRef[#["id"]]] , #|> &] ;

AppendTo[relations, {"azure.apiManagement.api"->"azure.apiManagement.product", {"azApiManagementProducts","azApiManagementProductList"}}];


(* ::Subsubsection:: *)
(*API Diagnostics*)


cfg = <|
	"azType"->"azure.apiManagement.api.diagnostic",
	"nameSingular"-> "ApiManagementApiDiagnostic",
	"namePlural"-> "ApiManagementApiDiagnostics",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData}, refData["apiName"]<>" / "<>refData["diagnosticId"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apidiagnostic",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/diagnostics/`diagnosticId`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/diagnostics?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.api"],
	"parentAzType" -> "azure.apiManagement.api",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> getIdKeyValue[res["id"],"apis/"],
		"diagnosticId" -> res["name"] 
		
	|>],
	"searchFields" -> {"name"},
	"microsoftType" -> "Microsoft.ApiManagement/service/apis",
	"microsoftIdTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`"
|>;
azureDefaultOperationsBuilder[cfg]


azApiManagementDiagnosticList[auth_, azRefAzurePattern["azure.apiManagement.logger"]] := Module[
	{loggerId, apimService ,diags, apis},
	loggerId = azInfo[auth,ref]["id"];
	apimService = azParent[auth, ref];
	apis = azApiManagementApis[auth,apimService];
	diags = Flatten[Normal@azApiManagementApiDiagnosticList[auth, #]& /@ apis] // Dataset;
	diags[Select[ToLowerCase[#["properties","loggerId"]] == ToLowerCase@loggerId &]]
];

AppendTo[relations, {"azure.apiManagement.logger"->"azure.apiManagement.api.diagnostic", {"azApiManagementDiagnostics","azApiManagementDiagnosticList"}}];


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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> getIdKeyValue[res["id"],"apis/"],
		"schemaName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg];


(* ::Subsection::Closed:: *)
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
		"subscriptionName" -> ref["subscriptionName"],
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


(* ::Subsection::Closed:: *)
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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"gatewayName" -> getIdKeyValue[res["id"],"gateways/"],
		"hostName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"diagnosticName" -> res["name"]
		
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
(*API Operations*)


(* ::Subsubsection:: *)
(*Operation*)


cfg = <|
	"azType"->"azure.apiManagement.api.operation",
	"nameSingular"-> "ApiManagementApiOperation",
	"namePlural"-> "ApiManagementApiOperations",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData},  refData["operationId"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apioperation",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/operations/`operationId`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/operations?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.api"],
	"parentAzType" -> "azure.apiManagement.api",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> getIdKeyValue[res["id"],"apis/"],
		"operationName" -> res["properties","displayName"],
		"operationId" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsubsection::Closed:: *)
(*Operation policy*)


cfg = <|
	"azType"->"azure.apiManagement.api.operation.policy",
	"nameSingular"-> "ApiManagementApiOperationPolicy",
	"namePlural"-> "ApiManagementApiOperationPolicies",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData},  refData["operationId"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apioperationpolicy",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/operations/`operationId`/policies/policy?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apis/`apiName`/operations/`operationId`/policies?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.api.operation"],
	"parentAzType" -> "azure.apiManagement.api.operation",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"apiName" -> getIdKeyValue[res["id"],"apis/"],
		"operationId" -> getIdKeyValue[res["id"],"operations/"],
		"policyName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
(*API Releases*)


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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"releaseId" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
(*API Version Sets*)


cfg = <|
	"azType"->"azure.apiManagement.api.versionSet",
	"nameSingular"-> "ApiManagementApiVersionSet",
	"namePlural"-> "ApiManagementApiVersionSets",
	"panelIcon"-> icons["azure.apiManagement"],
	"panelLabelFunc"-> Function[{refData},  refData["versionSetDisplayName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/apimanagement/2019-12-01/apiversionset",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apim-apis",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apiVersionSets/`versionSetName`?api-version=2019-12-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apiVersionSets?api-version=2019-12-01",
	"listFilter" -> azRefAzurePattern["azure.apiManagement.service"],
	"parentAzType" -> "azure.apiManagement.service",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"serviceName" -> getIdKeyValue[res["id"],"service/"],
		"versionSetName" -> res["name"],
		"versionSetDisplayName" -> res["properties","displayName"]
	|>],
	"searchFields" -> {"name"},
	"microsoftType" -> "Microsoft.ApiManagement/service/apiVersionSets",
	"microsoftIdTemplate" -> "/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.ApiManagement/service/`serviceName`/apiVersionSets/`versionSetName`"
|>;
azureDefaultOperationsBuilder[cfg]


azApiManagementApiVersionSetList[auth_, azRefAzurePattern["azure.apiManagement.api"]] := functionCatch["azApiManagementApiVersionSetList", Module[
	{api},
	api = azInfo[auth, ref]  // assertPattern[_Dataset];
	If[
		MatchQ[Normal@api, KeyValuePattern["properties"->KeyValuePattern["apiVersionSetId"->_String]]],
		{Normal@azInfo[auth, azMicrosoftIdToAzRef@api["properties","apiVersionSetId"]]} // Dataset,
		{}
	]
]];

AppendTo[relations, {"azure.apiManagement.api"->"azure.apiManagement.api.versionSet", {"azApiManagementApiVersionSets","azApiManagementApiVersionSetList"}}];


azApiManagementApiList[auth_, azRefAzurePattern["azure.apiManagement.api.versionSet"]] := 
	azApiManagementApiList[auth, azParent[auth, ref] ][Select[azApiManagementApiVersionSetList[auth, #["azRef"]] =!= {} &]];
	
AppendTo[relations, {"azure.apiManagement.api.versionSet"->"azure.apiManagement.api", {"azApiManagementApis","azApiManagementApiList"}}];


(* ::Section::Closed:: *)
(*Event Hubs*)


(* ::Subsubsection::Closed:: *)
(*Namespaces*)


cfg = <|
	"azType"->"azure.eventHub.namespace",
	"nameSingular"-> "EventHubNamespace",
	"namePlural"-> "EventHubNamespaces",
	"panelIcon"-> icons["azure.eventHub"],
	"panelLabelFunc"-> Function[{refData}, refData["namespace"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/eventhub/event-hubs-runtime-rest",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.EventHub/namespaces",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.EventHub/namespaces/`namespace`?api-version=2018-01-01-preview",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.EventHub/namespaces?api-version=2018-01-01-preview",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"namespace" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;
azureDefaultOperationsBuilder[cfg];


(* ::Section::Closed:: *)
(*Application Insights*)


(* ::Subsection::Closed:: *)
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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"componentName" -> res["name"],
		"appId" -> Association[res["properties"]]["AppId"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg];


(* ::Section::Closed:: *)
(*Virtual Networks*)


(* ::Subsection::Closed:: *)
(*Virtual Networks*)


(* ::Subsubsection::Closed:: *)
(*Virtual network*)


cfg = <|
	"azType"->"azure.virtualNetworks.network",
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
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"networkName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Subsubsection::Closed:: *)
(*Sub-nets*)


cfg = <|
	"azType"->"azure.virtualNetworks.subnets",
	"nameSingular"-> "VirtualNetworkSubnet",
	"namePlural"-> "VirtualNetworkSubnets",
	"panelIcon"-> icons["azure.virtualNetwork"],
	"panelLabelFunc"-> Function[{refData}, refData["subnetName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/virtualnetwork/subnets/get",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/virtualNetworks/`networkName`/subnets",
	"getUrl"-> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/virtualNetworks/`networkName`/subnets/`subnetName`?api-version=2020-05-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/virtualNetworks/`networkName`/subnets?api-version=2020-05-01",
	"listFilter" -> azRefAzurePattern["azure.virtualNetwork.network"],
	"parentAzType" -> "azure.virtualNetworks.network",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"networkName" -> getIdKeyValue[res["id"],"virtualNetworks/"],
		"subnetName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
(*Network interface*)


cfg = <|
	"azType"->"azure.virtualNetwork.nic",
	"nameSingular"-> "VirtualNetworkNic",
	"namePlural"-> "VirtualNetworkNics",
	"panelIcon"-> icons["azure.virtualNetwork.nic"],
	"panelLabelFunc"-> Function[{refData}, refData["interfaceName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/virtualnetwork/networkinterfaces",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/reourceGroupName/providers/Microsoft.Network/virtualNetworks/`networkName`/overview",
	"getUrl"-> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/networkInterfaces/`interfaceName`?api-version=2020-05-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Network/networkInterfaces?api-version=2020-05-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"interfaceName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]



azVirtualNetworkNicRouteTableList[auth_, azRefAzurePattern["azure.virtualNetwork.nic"]] := 
	azHttpPost[
		auth, {
			"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/networkInterfaces/`interfaceName`/effectiveRouteTable?api-version=2020-05-01",
			ref
		}
	] /. ds_Dataset :> ds;


(* ::Subsection::Closed:: *)
(*Public IP Addresses*)


cfg = <|
	"azType"->"azure.virtualNetwork.publicIp",
	"nameSingular"-> "VirtualNetworkPublicIp",
	"namePlural"-> "VirtualNetworkPublicIps",
	"panelIcon"-> icons["azure.virtualNetwork.publicIp"],
	"panelLabelFunc"-> Function[{refData}, refData["addressName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/virtualnetwork/publicipaddresses",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/reourceGroupName/providers/Microsoft.Network/virtualNetworks/`networkName`/overview",
	"getUrl"-> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/publicIPAddresses/`addressName`?api-version=2020-05-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Network/publicIPAddresses?api-version=2020-05-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"addressName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
(*Network Security Groups*)


cfg = <|
	"azType"->"azure.virtualNetwork.securityGroup",
	"nameSingular"-> "VirtualNetworkSecurityGroup",
	"namePlural"-> "VirtualNetworkSecurityGroups",
	"panelIcon"-> icons["azure.virtualNetwork.securityGroup"],
	"panelLabelFunc"-> Function[{refData}, refData["securityGroupName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/virtualnetwork/networksecuritygroups",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/networkSecurityGroups/`securityGroupName`/overview",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/networkSecurityGroups/`securityGroupName`?api-version=2020-05-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Network/networkSecurityGroups?api-version=2020-05-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"securityGroupName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
(*Route tables*)


cfg = <|
	"azType"->"azure.virtualNetwork.routeTable",
	"nameSingular"-> "VirtualNetworkRouteTable",
	"namePlural"-> "VirtualNetworkRouteTables",
	"panelIcon"-> icons["azure.virtualNetwork.routeTable"],
	"panelLabelFunc"-> Function[{refData}, refData["routeTableName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/virtualnetwork/routetables",
	"uiUrl" -> "/resource/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/routeTables/`routeTableName`/overview",
	"getUrl"->"https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.Network/routeTables/`routeTableName`?api-version=2020-05-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.Network/routeTables?api-version=2020-05-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"routeTableName" -> res["name"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Section::Closed:: *)
(*Key Vault*)


(* ::Subsection::Closed:: *)
(*Vaults*)


cfg = <|
	"azType"->"azure.keyVault",
	"nameSingular"->"KeyVault",
	"namePlural"->"KeyVaults",
	"panelIcon"-> icons["azure.keyVault"],
	"panelLabelFunc"-> Function[{refData}, refData["vaultName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/keyvault/vaults",
	"uiUrl" -> "/resource/subscriptions/`subscription`/resourceGroups/`resourceGroupName`/providers/Microsoft.KeyVault/vaults/`vaultName`/overview",
	"getUrl"-> "https://management.azure.com/subscriptions/`subscriptionId`/resourceGroups/`resourceGroupName`/providers/Microsoft.KeyVault/vaults/`vaultName`?api-version=2019-09-01",
	"listUrl" -> "https://management.azure.com/subscriptions/`subscriptionId`/providers/Microsoft.KeyVault/vaults?api-version=2019-09-01",
	"listFilter" -> azRefAzurePattern["azure.subscription"],
	"parentAzType" -> "azure.subscription",
	"listResultKeysFunc" -> Function[{res}, <|
		"subscriptionId" -> subscriptionIdFromId[res["id"]],
		"subscriptionName" -> ref["subscriptionName"],
		"resourceGroupName" -> resourceGroupNameFromId[res["id"]],
		"vaultName" -> res["name"],
		"vaultBaseUrl" -> res["properties","vaultUri"]
	|>],
	"searchFields" -> {"name"}
|>;

azureDefaultOperationsBuilder[cfg]


(* ::Subsection::Closed:: *)
(*Keys*)


azKeyVaultKeyList[auth_, azRefAzurePattern["azure.keyVault"]] := 
	azHttpGet[auth,
		ref["vaultBaseUrl"] <> "/keys?api-version=7.1"
	] ;


(* ::Subsection::Closed:: *)
(*Secrets*)


azKeyVaultSecretList[auth_, azRefAzurePattern["azure.keyVault"]] := 
	azHttpGet[auth,
		ref["vaultBaseUrl"] <> "/secrets?api-version=7.1"
	] ;


(* ::Section::Closed:: *)
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

azIcon[ref:azRef[refData:KeyValuePattern["azType"->"devOps.organization"]]] := azIcon[refData["azType"]];
azIcon["devOps.organization"] := icons["devOps.organization"];

azInfo[auth_, ref:azRef[KeyValuePattern["azType"->"devOps.organization"]]] := ref;


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
	"getUrl"->"https://dev.azure.com/`organizationName`/_apis/projects/`projectId`?api-version=6.0-preview.4",
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
|> // devOpsDefaultOperationsBuilder;


azDevOpsGitClone[auth_, azRefDevOpsPattern["devOps.git.repository"], folder_String] := Module[
	{repoInfo},
	repoInfo = azInfo[auth, ref];
	RunProcess[{"git","clone",repoInfo["webUrl"],folder}] /.
		KeyValuePattern["ExitCode" -> 0] :> folder
];


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


(* ::Subsection::Closed:: *)
(*Build*)


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Graph*)


(* ::Subsubsection::Closed:: *)
(*Storage key*)


azDevOpvStorageKey[auth_, azRefDevOpsPattern["devOps.user"]] :=Module[{res},
	azInfo[auth, ref] /. ds_Dataset :> azHttpGet[auth, ds["_links","storageKey","href"]]["value"]
] 


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


(* ::Subsection::Closed:: *)
(*Artifacts*)


(* ::Subsubsection::Closed:: *)
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



(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Task Agent*)


(* ::Subsubsection::Closed:: *)
(*Task Groups*)


 <|
	"azType"->"devOps.taskAgent.taskGroup",
	"nameSingular"->"AgentTaskGroup",
	"namePlural"->"AgentTaskGroups",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["taskGroupName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/distributedtask/taskgroups?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationNmae`/`projectId`/_taskgroup/`taskGroupId`",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/taskgroups/`taskGroupId`?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"projectId" -> ref["projectId"],
		"taskGroupName" -> res["name"],
		"taskGroupId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


azInfo[auth_, azRefDevOpsPattern["devOps.taskAgent.taskGroup"]] :=
	azDevOpsAgentTaskGroupList[auth,azParent[auth, ref]] /. ds_Dataset :> ds[Select[#id==ref["taskGroupId"]&]][1]


(* ::Subsubsection::Closed:: *)
(*Agent Pools*)


 <|
	"azType"->"devOps.taskAgent.agentPool",
	"nameSingular"->"AgentPool",
	"namePlural"->"AgentPools",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["poolName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/distributedtask/pools?view=azure-devops-rest-6.1",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_settings/agentqueues",
	"getUrl"->"https://dev.azure.com/`organizationName`/_apis/distributedtask/pools/`poolId`?api-version=6.1-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/_apis/distributedtask/pools?api-version=6.1-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.organization"],
	"parentAzType" -> "devOps.organization",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"poolName" -> res["name"],
		"poolId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Agents*)


<|
	"azType"->"devOps.taskAgent.agent",
	"nameSingular"->"Agent",
	"namePlural"->"Agents",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["agentName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/distributedtask/agents?view=azure-devops-rest-6.1",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_settings/agentqueues",
	"getUrl"->"https://dev.azure.com/`organizationName`/_apis/distributedtask/pools/`poolId`/agents/`agentId`?includeCapabilities=true&includeAssignedRequest=true,includeLastCompletedRequest=true&api-version=6.1-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/_apis/distributedtask/pools/`poolId`/agents?includeCapabilities=true&api-version=6.1-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.taskAgent.agentPool"],
	"parentAzType" -> "devOps.taskAgent.agentPool",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"poolId" -> ref["poolId"],
		"agentName" -> res["name"],
		"agentId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Environments*)


<|
	"azType"->"devOps.taskAgent.environment",
	"nameSingular"->"AgentEnvironment",
	"namePlural"->"AgentEnvironments",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["environmentName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/distributedtask/environments?view=azure-devops-rest-6.1",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_environments/`environmentId`?view=resources",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/environments/`environmentId`?api-version=6.1-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/environments?api-version=6.1-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"projectId" -> ref["projectId"],
		"environmentName" -> res["name"],
		"environmentId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Variable Groups*)


<|
	"azType"->"devOps.taskAgent.variableGroup",
	"nameSingular"->"AgentVariableGroup",
	"namePlural"->"AgentVariableGroups",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["groupName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/distributedtask/variablegroups?view=azure-devops-rest-6.1",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_library?itemType=VariableGroups",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/variablegroups/`groupId`?api-version=6.1-preview.2",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/variablegroups?api-version=6.1-preview.2",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"projectId" -> ref["projectId"],
		"groupName" -> res["name"],
		"groupId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder



azDevOpsAgentVariableGroupList[auth_, azRefDevOpsPattern["devOps.releaseDefinition"]] := functionCatch["azDevOpsAgentVariableGroupList", Module[
	{variableGroupIds, project, variableGroups},
	project = azParent[auth, ref] // assertPattern[_azRef];
	variableGroupIds = azInfo[auth, ref]["variableGroups"] // Normal // assertPattern[_List];
	variableGroups = azDevOpsAgentVariableGroupList[auth, project] // assertPattern[_Dataset];
	variableGroups[Select[MemberQ[variableGroupIds,#["id"]]&]]
]];

AppendTo[relations, {"devOps.releaseDefinition"->"devOps.taskAgent.variableGroup", {"azDevOpsAgentVariableGroups","azDevOpsAgentVariableGroupList"}}];


(* ::Subsubsection::Closed:: *)
(*Clouds*)


<|
	"azType"->"devOps.taskAgent.clouds",
	"nameSingular"->"AgentCloud",
	"namePlural"->"AgentClouds",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["cloudName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/distributedtask/agentclouds?view=azure-devops-rest-6.1",
	"getUrl"->"https://dev.azure.com/`organizationName`/_apis/distributedtask/agentclouds/`cloudId`?api-version=6.1-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/_apis/distributedtask/agentclouds?api-version=6.1-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"projectId" -> ref["projectId"],
		"cloudName" -> res["name"],
		"cloudId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Cloud types*)


azDevOpsAgentCloudTypes[auth_, azRefDevOpsPattern["devOps.organization"]] := 
	azHttpGet[auth, {
		"https://dev.azure.com/`organizationName`/_apis/distributedtask/agentcloudtypes?api-version=6.1-preview.1",
		ref
	}] /. ds_Dataset :> ds["value"]



(* ::Subsubsection::Closed:: *)
(*Deployment Groups*)


<|
	"azType"->"devOps.taskAgent.deploymentGroups",
	"nameSingular"->"AgentDeploymentGroup",
	"namePlural"->"AgentDeploymentGroups",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["groupName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/distributedtask/deploymentgroups?view=azure-devops-rest-6.1",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/deploymentgroups/`groupId`?api-version=6.1-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/deploymentgroups?api-version=6.1-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"projectId" -> ref["projectId"],
		"groupName" -> res["name"],
		"groupId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Subsubsection::Closed:: *)
(*Queues*)


<|
	"azType"->"devOps.taskAgent.queue",
	"nameSingular"->"AgentQueue",
	"namePlural"->"AgentQueues",
	"panelIcon"-> icons["devOps.pipeline"],
	"panelLabelFunc"-> Function[{refData},refData["queueName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/distributedtask/queues?view=azure-devops-rest-6.1",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/queues/`queueId`?api-version=6.1-preview.1",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/distributedtask/queues?api-version=6.1-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"projectId" -> ref["projectId"],
		"queueName" -> res["name"],
		"queueId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder

azDevOpsAgentQueueList[auth_, azRefDevOpsPattern["devOps.releaseDefinition"]] := functionCatch["azDevOpsAgentQueueList", Module[
	{queueIds, project, queues},
	queueIds = azInfo[auth, ref]
		["environments",All,"deployPhases",All,"deploymentInput","queueId"] // 
			Normal // Flatten // DeleteDuplicates // assertPattern[{_Integer...}];
	project =  azParent[auth, ref] // assertPattern[_azRef];
	queues = azDevOpsAgentQueueList[auth, project] // assertPattern[_Dataset];
	queues[Select[MemberQ[queueIds,#["id"]]&]]
]]
	
AppendTo[relations, {"devOps.releaseDefinition"->"devOps.taskAgent.queue", {"azDevOpsAgentQueues","azDevOpsAgentQueueList"}}];


(* ::Subsubsection:: *)
(*Virtual Machines*)


(* ::Subsection::Closed:: *)
(*Service Endpoints*)


 <|
	"azType"->"devOps.serviceEndpoint",
	"nameSingular"->"ServiceEndpoint",
	"namePlural"->"ServiceEndpoints",
	"panelIcon"-> icons["devOps.serviceEndpoint"],
	"panelLabelFunc"-> Function[{refData},refData["endpointName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/serviceendpoint/endpoints?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_settings/adminservices",
	"getUrl"->"https://dev.azure.com/`organizationName`/`projectId`/_apis/serviceendpoint/endpoints/`endpointId`?api-version=6.0-preview.4",
	"listUrl" -> "https://dev.azure.com/`organizationName`/`projectId`/_apis/serviceendpoint/endpoints?api-version=6.0-preview.4",
	"listFilter" -> azRefDevOpsPattern["devOps.project"],
	"parentAzType" -> "devOps.project",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"projectId" -> ref["projectId"],
		"endpointName" -> res["name"],
		"endpointId" -> res["id"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


azDevOpsServiceEndpointExecutionHistory[auth_, azRefDevOpsPattern["devOps.serviceEndpoint"]] := 
	azHttpGet[auth, {
		"https://dev.azure.com/`organizationName`/`projectId`/_apis/serviceendpoint/`endpointId`/executionhistory?api-version=6.0-preview.1",
		ref
	}] /. ds_Dataset :> ds["value"]


azDevOpsServiceEndpointTypes[auth_, azRefDevOpsPattern["devOps.organization"]] := 
	azHttpGet[auth, {
		"https://dev.azure.com/`organizationName`/_apis/serviceendpoint/types?api-version=6.0-preview.1",
		ref
	}] /. ds_Dataset :> ds["value"]


(* ::Subsection::Closed:: *)
(*Extension Management*)


 <|
	"azType"->"devOps.extension",
	"nameSingular"->"InstalledExtension",
	"namePlural"->"InstalledExtensions",
	"panelIcon"-> icons["devOps.extension"],
	"panelLabelFunc"-> Function[{refData},refData["extensionName"]],
	"restDocumentation"->"https://docs.microsoft.com/en-us/rest/api/azure/devops/extensionmanagement/installed%20extensions?view=azure-devops-rest-6.0",
	"uiUrl" -> "https://dev.azure.com/`organizationName`/_settings/extensions?tab=installed",
	"getUrl"->"https://extmgmt.dev.azure.com/`organizationName`/_apis/extensionmanagement/installedextensionsbyname/`publisherId`/`extensionId`?api-version=6.0-preview.1",
	"listUrl" -> "https://extmgmt.dev.azure.com/`organizationName`/_apis/extensionmanagement/installedextensions?api-version=6.0-preview.1",
	"listFilter" -> azRefDevOpsPattern["devOps.organization"],
	"parentAzType" -> "devOps.organization",
	"listResultKeysFunc" -> Function[{res}, <| 
		"organizationName" -> ref["organizationName"], 
		"extensionName" -> res["extensionName"],
		"extensionId" -> res["extensionId"],
		"publisherName" ->res["publisherName"],
		"publisherId" -> res["publisherId"]
	|>],
	"searchFields" -> {"name"}
|> // devOpsDefaultOperationsBuilder


(* ::Section::Closed:: *)
(*Footer*)


End[];


EndPackage[]
