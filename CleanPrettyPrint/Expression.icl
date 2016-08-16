implementation module CleanPrettyPrint.Expression

import StdEnv

import CleanPrettyPrint.Util
import CleanPrettyPrint.Common
import CleanPrettyPrint.Definition

import syntax

// General expressions
instance print ParsedExpr
where
	print st (PE_List [(PE_Ident {id_name}),a,b])
		| id_name == "_Cons"
			= "[" +++ rest
		| id_name == "_cons"
			= "[|" +++ rest
		| id_name.[0] == '_' && id_name % (2,6) == "Cons"
			= "[" +++ {id_name.[1]} +++ rest
	where
		rest = print st a +++ ":" +++ print st b +++ "]"
	print st (PE_List pes)
		= printp st pes
	print st (PE_Ident id)
		= print st id
	print st (PE_Basic b)
		= print st b
	print st (PE_Tuple pes)
		= "(" +++ join st "," pes +++ ")"
	print st (PE_ArrayDenot ak elems)
		= print st ("{" :+: ak :+: join st "," elems :+: "}")
	print st (PE_Record init name fields)
		= print st ("{ " :+: name` :+: init` :+: join st ", " fields :+: " }")
	where
		init` = case init of
			PE_Empty = ""
			_        = print st init +++ " & "
		name` = case name of
			NoRecordName         = ""
			(RecordNameIdent id) = print st id +++ " | "
			_                    = abort "UNKNOWN_OPTIONALRECORDNAME"
	print st (PE_ListCompr cons nil pe qs)
		= print st ("[" :+: pe :+: " \\\\ " :+: join st ", " qs :+: "]")
	print st (PE_If _ c i e)
		= "if " +++ join { st & cpp_parens=True } " " [c,i,e]
	print st (PE_Case _ pe alts)
		= "case " +++ print {st & cpp_parens=True} pe +++ " of" +++ join_start st` ("\n" :+: st`) alts
	where
		st` = {st & cpp_indent = st.cpp_indent + 1}
	print st (PE_Sequ seq)
		= print st seq
	print st (PE_Lambda _ pes rhs _)
		= printp st ("\\" :+: join st " " pes :+: " -> " :+: rhs)
	print st (PE_Let lds pe)
		= printp st ("let " :+: join st ", " lds :+: " in " :+: pe)
	print st (PE_Bound {bind_src,bind_dst})
		= print st (bind_dst :+: "=:" :+: bind_src)
	print st PE_WildCard
		= "_"
	print st (PE_Update e1 sels e2)
		= print {st & cpp_parens=False} ("{" :+: e1 :+: " & " :+: printParsedSelections st sels :+: "=" :+: e2 :+: "}")
	print st (PE_Selection psk pe pes)
		= print st (pe :+: sel :+: printParsedSelections st pes)
	where
		sel = case psk of
			ParsedNormalSelector     = "."
			(ParsedUniqueSelector _) = "!"
	// | PE_ArrayPattern ![ElemAssignment]
	// | PE_UpdateComprehension !ParsedExpr !ParsedExpr !ParsedExpr ![Qualifier]
	// | PE_ArrayCompr !ArrayKind !ParsedExpr ![Qualifier]
	// | PE_Matches !Ident /*expr*/!ParsedExpr /*pattern*/!ParsedExpr !Position
	// | PE_QualifiedIdent !Ident !String
	// | PE_ABC_Code ![String] !Bool
	// | PE_Any_Code !(CodeBinding Ident) !(CodeBinding Ident) ![String]
	// | PE_DynamicPattern !ParsedExpr !DynamicType
	// | PE_Dynamic !ParsedExpr !(Optional DynamicType)
	// | PE_Generic !Ident !TypeKind	/* AA: For generics, kind indexed identifier */
	// | PE_TypeSignature !ArrayKind !ParsedExpr
	// | PE_Empty
	print st pe
		= abort "UNKNOWN_PE"

printParsedSelections :: CPPState [ParsedSelection] -> String
printParsedSelections st [PS_Array pe] = print {st & cpp_parens=False} ("[" :+: pe :+: "]")
printParsedSelections st _             = "UNKNOWN_PARSEDSELECTION"

instance print Rhs
where
	print st {rhs_alts,rhs_locals=LocalParsedDefs []}
		= print st rhs_alts
	print st {rhs_alts,rhs_locals}
		= let st` = {st & cpp_indent = st.cpp_indent + 1} in
			print st (rhs_alts :+: "\n" :+: st :+: "where\n" :+: st` :+: join st` ("\n" :+: st`) rhs_locals)

// Basic values
instance print BasicValue
where
	print _ (BVInt i) = toString i
	print _ (BVC c)   = c
	print _ (BVB b)   = toString b
	print _ (BVR r)   = r
	print _ (BVS s)   = s
	print _ (BVI _)   = "BVI???"

// Lists
instance print Qualifier
where
	print st q=:{qual_filter=Yes filt} = print st ({q & qual_filter=No} :+: " | " :+: filt)
	print st q=:{qual_generators} = join st " & " qual_generators

instance print Generator
where
	print st {gen_pattern,gen_expr,gen_kind}
		= print st (gen_pattern :+: select :+: gen_expr)
	where
		select = case gen_kind of
			IsListGenerator = " <- "
			IsOverloadedListGenerator = " <|- "
			IsArrayGenerator = " <-: "

instance print Sequence
where
	print st (SQ_FromThen i e1 e2)
		= print st ("[" :+: e1 :+: "," :+: e2 :+: "..]")
	print st (SQ_FromThenTo i e1 e2 e3)
		= print st ("[" :+: e1 :+: "," :+: e2 :+: ".." :+: e3 :+: "]")
	print st (SQ_From i e)
		= print st ("[" :+: e :+: "..]")
	print st (SQ_FromTo i e1 e2)
		= print st ("[" :+: e1 :+: ".." :+: e2 :+: "]")

// Arrays
instance print ArrayKind
where
	print _ OverloadedArray = ""
	print _ StrictArray     = "!"
	print _ UnboxedArray    = "#"

// Records
instance print FieldAssignment
where
	print st {bind_src,bind_dst} = print st (bind_dst :+: "=" :+: bind_src)

instance print FieldNameOrQualifiedFieldName
where
	print st (FieldName id)             = print st id
	print st (QualifiedFieldName mod s) = abort "UNKNOWN_QUALIFIEDFIELDNAME"

// Case .. of
instance print CaseAlt
where
	print st ca = print st (ca.calt_pattern :+: " = " :+: ca.calt_rhs)

// Local definitions
instance Join LocalDefs
where
	join st glue (LocalParsedDefs lds) = join st glue lds
	join st glue _                     = abort "JOIN: UNKNOWN_LOCALDEFS"

	isNil (LocalParsedDefs []) = True
	isNil (LocalParsedDefs _)  = False
	isNil _                    = abort "JOIN: UNKNOWN_LOCALDEFS"

instance print ExprWithLocalDefs
where
	print st {ewl_expr,ewl_locals=LocalParsedDefs []}
		= print st ewl_expr
	print st {ewl_expr,ewl_locals}
		= print st (ewl_expr :+: "\n" :+: st` :+: "with" :+: join_start st`` ("\n" :+: st``) ewl_locals)
	where
		st`  = {st & cpp_indent = st.cpp_indent + 1}
		st`` = {st & cpp_indent = st.cpp_indent + 2}

// Guards
instance print OptGuardedAlts
where
	print st (GuardedAlts ges (Yes othe))
		= print st (join_start st ("\n" :+: st :+: "| ") ges :+: "\n" :+: st :+: "| otherwise = " :+: othe)
	print st (GuardedAlts ges No)
		= join_start st ("\n" :+: st :+: "| ") ges
	print st (UnGuardedExpr e)
		= print st e

instance print GuardedExpr
where
	print st {alt_guard,alt_expr}
		= print {st & cpp_indent = st.cpp_indent + 1} alt_guard +++ eq +++ print st alt_expr
	where
		eq = case alt_expr of (GuardedAlts _ _) = ""; _ = " = "
