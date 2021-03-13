(* ::Package:: *)

BeginPackage["cert`"];


certGetChain;
certOsRoots;


Begin["`Private`"];


certGetChain[url_String] := Module[{process, output},
	process = StartProcess[{"openssl", "s_client","-showcerts","-connect",url}];
	If[ProcessStatus@process == "Running",
		output=ReadString[process, "Server certificate"];
		WriteLine[process, "Q\n"];
		output /. out_String :> Flatten[
			ImportString[#,"PEM"]& /@ StringCases[out,
										"-----BEGIN CERTIFICATE-----"~~(Except["-"]..)~~"-----END CERTIFICATE-----"
										]],
		Null
	]
]


certOsRoots[] := RunProcess[
	{"security","find-certificate","-a","-p","/System/Library/Keychains/SystemRootCertificates.keychain"}
] /. KeyValuePattern[{
	"ExitCode"->0,
	"StandardOutput"->std_String
}] :> Flatten[
	ImportString[#,"PEM"]& /@ StringCases[std,"-----BEGIN CERTIFICATE-----"~~(Except["-"]..)~~"-----END CERTIFICATE-----"]]


End[];


EndPackage[];
