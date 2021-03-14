(* ::Package:: *)

BeginPackage["oauth`"];


oauthCodeFlowAuthorize;
oauthCodeFlowToken;
oauthIntrospection;
oauthRefreshToken


Begin["`Private`"];


oauthCodeFlowAuthorize[KeyValuePattern[{
	"authorization_endpoint"->authBase_String,
	"redirect"->redirectBase_String,
	"client_id"->clientId_String
}], scopes:{_String...}] := Module[
	{browserSesion,authorizeUrl,redirectUrl,state,res},
	state=ResourceFunction["RandomString"][8];
	authorizeUrl=URLBuild[authBase,{
		"client_id"->clientId,
		"response_type"->"code",
		"redirect_uri"->redirectBase,
		"scope"->StringRiffle[scopes," "],
		"state"->state
	}];
	browserSesion=StartWebSession[];
	WebExecute[browserSesion,"OpenPage"->authorizeUrl];
	(* Wait for redirect to callback *)
	While[!StringContainsQ[WebExecute[browserSesion,"PageURL"],redirectBase],Pause[1]];
	redirectUrl=WebExecute[browserSesion,"PageURL"];
	DeleteObject[browserSesion];
	res=<|URLParse[redirectUrl]["Query"]|>;
	If[res["state"]===state,res,Failure["Failed to match state",<||>]]
];

oauthCodeFlowToken[params_Association,KeyValuePattern["code"->code_String]] :=
	oauthCodeFlowToken[params, code];
oauthCodeFlowToken[KeyValuePattern[{
	"token_endpoint"->tokenBase_String,
	"client_id"->clientId_String,
	"client_secret"->clientSecret_,
	"redirect"->redirectBase_String
}], code_String] := Module[
	{req,res},
	req=HTTPRequest[tokenBase,<|
		Method->"POST",
		"ContentType"->"application/x-www-form-urlencoded",
		"Body"->URLQueryEncode[<|
			"grant_type"->"authorization_code",
			"redirect_uri"->redirectBase,
			"code"->code
		|>],
		"Headers"->{
			"Accept"->"application/json",
			"Authorization"->"Basic "<>BaseEncode@StringToByteArray[clientId<>":"<>clientSecret]
		}
	|>];
	res=URLRead@req;
	res/.r:HTTPResponse[_,KeyValuePattern["StatusCode"->200],___]:> ImportString[r["Body"],"RawJSON"]
];

oauthIntrospection[KeyValuePattern[{
	"introspection_endpoint" -> introspectUrl_String,
	"client_id" -> clientId_String,
	"client_secret" -> clientSecret_String
 }], token_String] := Module[
	{req,res},
	req=HTTPRequest[introspectUrl,<|
		Method->"POST",
		"ContentType"->"application/x-www-form-urlencoded",
		"Body"->URLQueryEncode[<|"token"->token|>],
		"Headers"->{
			"Accept"->"application/json",
			"Authorization"->"Basic "<>BaseEncode@StringToByteArray[clientId<>":"<>clientSecret]
		}
	|>];
	res=URLRead@req;
	res/.r:HTTPResponse[_,KeyValuePattern["StatusCode"->200],___]:> ImportString[r["Body"],"RawJSON"]
];


oauthRefreshToken[KeyValuePattern[{
	"token_endpoint"->tokenBase_String,
	"client_id"->clientId_String,
	"client_secret"->clientSecret_,
	"redirect"->redirectBase_String
}], rtoken_String] := Module[
	{req,res},
	req=HTTPRequest[tokenBase,<|
		Method->"POST",
		"ContentType"->"application/x-www-form-urlencoded",
		"Body"->URLQueryEncode[<|
			"grant_type"->"refresh_token",
			"refresh_token"->rtoken
		|>],
		"Headers"->{
			"Accept"->"application/json",
			"Authorization"->"Basic "<>BaseEncode@StringToByteArray[clientId<>":"<>clientSecret]
		}
	|>];
	res=URLRead@req;
	res/.r:HTTPResponse[_,KeyValuePattern["StatusCode"->200],___]:> ImportString[r["Body"],"RawJSON"]
];


End[];


EndPackage[];
