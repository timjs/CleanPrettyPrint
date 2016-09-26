implementation module CleanPrettyPrint.Definition

import StdEnv

import CleanPrettyPrint.Util
import CleanPrettyPrint.Common
import CleanPrettyPrint.Expression

import syntax

instance print ParsedDefinition
where
	print st (PD_Import ips)
		= join st "\n" ips
	print st (PD_Class cd mems)
		= print st ("class " :+: cd.class_ident :+: args :+: context :+: if (isEmpty mems) "" " where" :+: join_start st` ("\n" :+: st`) mems)
	where
		st` = { st & cpp_indent = st.cpp_indent + 1 }
		context = if (isEmpty cd.class_context) "" (" | " +++ join st " & " cd.class_context)
		args = if (isEmpty cd.class_args) "" (join_start st " " cd.class_args)
    print st (PD_Instance {pim_pi={pi_ident,pi_types,pi_context},pim_members})
		= print st ("instance " :+: pi_ident :+: " " :+: join st ", " pi_types :+: pi_context` :+: members)
	where
		pi_context` = if (isEmpty pi_context) PrintNil (" | " :+: join st " & " pi_context)
		members = if (isEmpty pim_members) PrintNil (" where" :+: join_start st` ("\n" :+: st`) pim_members)
		st` = {st & cpp_indent = st.cpp_indent + 1}
    print st (PD_Instances pis=:[{pim_pi={pi_ident}}:_])
		= print st ("instance " :+: pi_ident :+: " " :+: join st ", " (map (\i -> i.pim_pi.pi_types) pis))
	print st (PD_Generic {gen_ident,gen_type,gen_vars})
		= print st ("generic " :+: gen_ident :+: join_start st " " gen_vars :+: " :: " :+: gen_type)
	print st (PD_Derive gencasedefs)
		= print st ("derive " :+: join st ", " gencasedefs)
    print st (PD_TypeSpec pos id prio type funspecs)
		= print st (id` :+: prio` :+: type`)
	where
		id` = case prio of
			NoPrio = id :+: PrintNil
			_      = "(" :+: id :+: ")"
		prio` = case prio of
			(Prio LeftAssoc p)  = " infixl " :+: p
			(Prio RightAssoc p) = " infixr " :+: p
			(Prio NoAssoc p)    = " infix "  :+: p
			NoPrio              = PrintNil
		type` = case type of
			(Yes t) = " :: " :+: t
			No      = PrintNil
	print st (PD_NodeDef _ l r)
		= print st (l :+: " = " :+: r)
	print st (PD_Function _ id isinfix args rhs fk)
		= print st (id` :+: join_start st " " args :+: if show_eq eq "" :+: rhs)
	where
		id` = if isinfix ("(" :+: id :+: ")") (id :+: PrintNil)
		show_eq = not (compound_rhs rhs.rhs_alts)
		eq = case fk of FK_Macro = " :== "; _ = " = "
	print st (PD_Type {td_ident,td_args,td_attribute,td_rhs})
		= print st (":: " :+: td_attribute :+: td_ident :+: join_start st " " td_args :+: equals :+: td_rhs)
	where
		equals = case td_rhs of
			(TypeSpec _) = " :== "
			(EmptyRhs _) = ""
			_            = " = "
	print _ _
		= abort "UNKNOWN_PD"

// General types
instance print BasicType
where
	print st BT_Int = "Int"
	print st BT_Char = "Char"
	print st BT_Real = "Real"
	print st BT_Bool = "Bool"
	print st (BT_String t) = print st ("String" :+: t)
	print st BT_File = "File"
	print st BT_World = "World"
	print st BT_Dynamic = "Dynamic"

instance print SymbolType
where
	print st t
		= print st (if (isEmpty t.st_args) PrintNil (args` :+: " -> ") :+: t.st_result :+: st_context` :+: st_env`)
	where
		st_context` = if (isEmpty t.st_context) PrintNil (" | " :+: join st " & " t.st_context)
		st_env` = if (isEmpty t.st_attr_env) PrintNil (", [" :+: join st ", " t.st_attr_env :+: "]")
		args` = join st " " [if s "!" "" :+: a \\ a <- t.st_args & s <- strictnessListToBools t.st_args_strictness]

strictnessListToBools :: StrictnessList -> [Bool]
strictnessListToBools NotStrict        = repeat False
strictnessListToBools (Strict i)       = [i bitand (1 << e) <> 0 \\ e <- [0..31]]
strictnessListToBools (StrictList i l) = strictnessListToBools (Strict i) ++ strictnessListToBools l

instance print Type
where
	print st (TA tsi ats)
		= print st (tsi, ats, [False \\ _ <- ats])
	print st (TAS tsi ats slist)
		= print st (tsi, ats, strictnessListToBools slist)
	print st (at1 --> at2)
		= print st ("(" :+: at1 :+: " -> " :+: at2 :+: ")")
	print st TArrow
		= "(->)"
	print st (TArrow1 at)
		= print st ("((->) " :+: at :+: ")")
	print st (cv :@: ats)
		= print st ("(" :+: cv :+: " " :+: join st " " ats :+: ")")
	print st (TB bt)
		= print st bt
	//print st (TFA atvs type)
	//	= "TFA"
	print st (GTV tv)
		= print st (tv :+: "^")
	print st (TV tv)
		= print st tv
	print st (TFAC atvs t tc)
		= print st ("(A." :+: join st " " atvs :+: ": " :+: t :+: " | " :+: join st " & " tc :+: ")")
	print st (TQualifiedIdent id s [])
		= print st ("'" :+: id :+: "'." :+: s)
	print st (TQualifiedIdent id s ats)
		= print st ("('" :+: id :+: "'." :+: s :+: join_start st " " ats :+: ")")
	//|	TGenericFunctionInDictionary !(Global DefinedSymbol) !TypeKind !GlobalIndex /*GenericDict*/
	//|	TE
	print st _
		= abort "UNKNOWN_TYPE"

instance print ConsVariable where print st (CV tv) = print st tv //TODO

instance print TypeVar where print st {tv_ident} = tv_ident.id_name

instance print AType
where
	print st {at_attribute=TA_Var {av_ident},at_type=TV {tv_ident}}
	| av_ident.id_name == tv_ident.id_name = "." +++ tv_ident.id_name
	print st at = print st (at.at_attribute :+: at.at_type)

instance print ATypeVar
where
	print st v = print st (v.atv_attribute :+: v.atv_variable)

instance print TypeAttribute where print _ a = toString a

instance print (TypeSymbIdent, [AType], /* is_strict */ [Bool])
where
	print st (tsi, ats, strict)
		= print st (case lookup tsi.type_ident.id_name of
			(Yes s) = s
			No      = case ats of
				[]  = tsi :+: PrintNil
				_   = "(" :+: tsi :+: " " :+: join st " " ats :+: ")"
		)
	where
		lookup "_String"           = Yes ("String" :+: PrintNil)
		lookup "_Unit"             = Yes ("()" :+: PrintNil)
		lookup "_List"             = Yes ("["  :+: join st " " ats :+:  "]")
		lookup "_!List"            = Yes ("[!" :+: join st " " ats :+:  "]")
		lookup "_List!"            = Yes ("["  :+: join st " " ats :+: "!]")
		lookup "_!List!"           = Yes ("[!" :+: join st " " ats :+: "!]")
		lookup "_|List"            = Yes ("[|" :+: join st " " ats :+:  "]")
		lookup "_#List"            = Yes ("[#" :+: join st " " ats :+:  "]")
		lookup "_#List!"           = Yes ("[#" :+: join st " " ats :+: "!]")
		lookup "_Array"            = Yes ("{"  :+: join st " " ats :+:  "}")
		lookup "_#Array"           = Yes ("{#" :+: join st " " ats :+:  "}")
		lookup "_!Array"           = Yes ("{!" :+: join st " " ats :+:  "}")
		lookup name
		| name % (0,5) == "_Tuple" = Yes ("(" :+: join st "," [if s "!" "" :+: a \\ a <- ats & s <- strict] :+: ")")
		lookup _                   = No

// Type contexts
instance print TypeContext
where
	print st tc
		= print st (tc.tc_class :+: " " :+: join st ", " tc.tc_types)

// Type definitions
instance print RhsDefsOfType
where
	print st (ConsList conses)
		= join st " | " conses
	print st (SelectorList _ exivars _ fields)
		= print st (exivars` :+: "{" :+: join st ", " fields :+: "}")
	where
		exivars` = if (isEmpty exivars) PrintNil ("E." :+: join st " " exivars :+: ": ")
	print st (TypeSpec type)
		= print st type
	print st (EmptyRhs _)
		= ""
	print _ _
		= abort "UNKNOWN_RHSDEFSOFTYPE"

instance print ParsedSelector
where
	print st ps = print st (ps.ps_selector_ident :+: " :: " :+: ps.ps_field_type)

instance print ParsedConstructor
where
	print st cons = print st (cons.pc_cons_ident :+: " " :+: cons.pc_arg_types)

// Classes
instance print TCClass
where
	print st (TCClass {glob_object={ds_ident}})
		= print st ds_ident
	print st (TCGeneric {gtc_generic,gtc_kind})
		= print st (gtc_generic.glob_object.ds_ident.id_name :+: "{|" :+: gtc_kind :+: "|}")
	print st _
		= abort "UNKNOWN_TCCLASS"

// Generics
instance print GenericCaseDef
where
	print st {gc_type,gc_gcf=GCF id _}
		= print st (id :+: " " :+: gc_type)
	print st {gc_type,gc_gcf=GCFC id _}
		= print st ("class " :+: id :+: " " :+: gc_type)
	print _ _
		= abort "UNKNOWN_GENERICCASEDEF"

instance print TypeKind
where
	print st KindConst = print st "*"
	print st (KindArrow ks) = print st ("*->" :+: ks)

// Uniqueness
instance print AttrInequality
where
	print st ai = print st (ai.ai_offered.av_ident :+: "<=" :+: ai.ai_demanded.av_ident)

// Miscellaneous
instance print TypeSymbIdent where print st tsi = print st tsi.type_ident
