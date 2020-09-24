(* ::Package:: *)

BeginPackage["viewers`"];


view::usage="View and explore data";


Begin["`Private`"];


view[l_List]:=Column[view[#]& /@ l];
view[json[s_String]]:=ImportString[s,"RawJSON"]//view;
view[v_]:=v;
view[noView[v_]]:=v;
view[a_Association]:=Framed[Column[associationSubView[#]&/@Normal[a]],RoundingRadius->5];
associationSubView[k_->v_]:=OpenerView[{k,Dynamic[view@v]}];
associationSubView[k_->v:(_String|_Integer|_Real)]:= k <> ": " <> ToString[v];

(* XML view *)
view[XMLObject[type_String][inner___]] :=
	OpenerView[{type<> " (xml object)",Dynamic[view@{inner}]}];
view[XMLElement[tag_String|{ns_String,tag_String},attr_List,data_List]] :=
	Framed@OpenerView[{
		ns<>":"<>tag <> " (xml tag)",
		Dynamic@Column[{
			OpenerView[{"attribues",view@Association@attr}],
			OpenerView[{"body",view@data}]
		}]
	}];
	
view[TemplateObject[t_]] := view@t;


End[];


EndPackage[]
