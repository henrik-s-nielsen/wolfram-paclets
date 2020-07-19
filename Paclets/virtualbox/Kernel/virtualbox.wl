(* ::Package:: *)

(* ::Title:: *)
(*Virtual box*)


(* ::Section:: *)
(*Header*)


BeginPackage["virtualbox`"];


$vboxProgram;
vboxManageRun;
vboxListVms;
vboxListNatNetworks;
vboxVmInfo;
vboxCreateVm;
vboxDeleteVm;
vboxOsTypes;
vboxModifyVm;
vboxStartVm;
vboxControlVmPowerOff;


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


$vboxProgram = "/Applications/VirtualBox.app/Contents/MacOS/VBoxManage";

Attributes[parseKeyValue]={HoldAll};
Quiet[parseKeyValue[txtKey_String, sym_Symbol]:=txtKey~~":"~~(" "..)~~sym:(Except["\n"]..)~~"\n"]

vboxManageRun[arg_List] :=
	RunProcess[{
		$vboxProgram,
		Sequence@@arg
}] /. KeyValuePattern[{"ExitCode"->0,"StandardOutput"->s_}] :> s

vboxListVms[] := Module[
	{runningVmsTxt},
	runningVmsTxt = vboxManageRun[{"list","runningvms"}];
	vboxManageRun[{"list","vms"}] /. 
		s_String :> Dataset@StringCases[
			s,
			"\"" ~~ name:(Except["\""]..) ~~ "\" " ~~ "{" ~~ id:(Except["}"]..) ~~ "}\n" :> 
				<|
					"name" -> name,
					"UUID" -> id,
					"running" -> StringContainsQ[runningVmsTxt,id]
				|>]
]

vboxListNatNetworks[]:=vboxManageRun[{"list","natnets"}] /. s_String :>
	Module[
		{name,ip,network,ipv6Enabled,ipv6Prefix,dhcpEnabled,enabled},
		Dataset@StringCases[
			s,
			parseKeyValue["NetworkName",name] ~~
			parseKeyValue["IP",ip] ~~
			parseKeyValue["Network",network] ~~
			parseKeyValue["IPv6 Enabled",ipv6Enabled] ~~
			parseKeyValue["IPv6 Prefix",ipv6Prefix] ~~
			parseKeyValue["DHCP Enabled",dhcpEnabled] ~~
			parseKeyValue["Enabled",enabled] :>
				<|
					"name"->name,
					"ip"->ip,
					"network"->network,
					"ipv6Enabled" -> ipv6Enabled /. {"No"->False,"Yes"->True},
					"dhcpEnabled" -> dhcpEnabled /. {"No"->False,"Yes"->True},
					"loopbackMapping"->"[not parsed]"
				|>
]]

vboxVmInfo[d_Dataset] := vboxVmInfo[Normal@d];
vboxVmInfo[KeyValuePattern["name"->n_]] := vboxVmInfo[n];
vboxVmInfo[KeyValuePattern["UUID"->u_]] := vboxVmInfo[u];
vboxVmInfo[idName_String] := vboxManageRun[{"showvminfo", idName, "--machinereadable"}] /. 
	(s_String :> ((StringSplit[#,"="]/.{k_,v_}:>(StringReplace[k,"\""->""]->(Quiet@ToExpression@v)))&) /@ StringSplit[StringReplace[s,"=="->"="],"\n"]) // Association
	
vboxCreateVm[name_String] := vboxManageRun[{
	"createvm",
	"--name", name,
	"--register"
}] /. s_String :> <|
		"name" -> name,
		"UUID" -> StringCases[s, "UUID: "~~u:(Except["\n"]..):>u] /. {v_} :> v 
	|>
vboxCreateVm[name_String, a_Association] := Module[{},
	vboxCreateVm[name];
	vboxModifyVm[name, a]
]
	
vboxDeleteVm[idName_String] := vboxManageRun[{"unregistervm", idName, "--delete"}]

vboxOsTypes[] := vboxManageRun[{"list","ostypes"}] /. s_String :> Module[
	{id,desc,familyId,familyDesc,bit64},
	Dataset@StringCases[s,
		parseKeyValue["ID",id] ~~
		parseKeyValue["Description",desc] ~~
		parseKeyValue["Family ID",familyId] ~~
		parseKeyValue["Family Desc",familyDesc] ~~
		parseKeyValue["64 bit",bit64] :>
			<|
				"id" -> id,
				"description" -> desc,
				"familyId" -> familyId,
				"familyDesc" -> familyDesc,
				"bit64" -> bit64 /. {"false"->False,"true"->True}
			|>
]]

vboxModifyVm[idName_String, key_String, value_String] := 
	vboxManageRun[{"modifyvm",idName, "--"<>key, "\""<>value<>"\""}] /. "" -> Null
vboxModifyVm[idName_String, key_String, value_String] :=
	vboxManageRun[{"modifyvm",idName, "--"<>key,value}] /. "" -> Null
vboxModifyVm[idName_String, key_String -> value_String] := vboxModifyVm[idName,key,value]
vboxModifyVm[idName_String, l_List] := vboxModifyVm[idName,#] & /@ l
vboxModifyVm[idName_String, a_Association] := vboxModifyVm[idName, Normal@a]

Options[vboxStartVm] := {"vmType"->"gui"}
vboxStartVm[idName_String, OptionsPattern[]] := vboxManageRun[{"startvm", idName, "--type", OptionValue["vmType"]}]

vboxControlVmPowerOff[idName_String] := vboxManageRun[{"controlvm", idName, "poweroff"}]


(* ::Section:: *)
(*Footer*)


End[];


EndPackage[]
