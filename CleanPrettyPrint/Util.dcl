definition module CleanPrettyPrint.Util

from StdOverloaded import class zero, class +++(+++)

from StdList import isEmpty

:: CPPState =
	{ cpp_indent :: Int
	, cpp_parens :: Bool
	}

:: PrintList = PrintNil
             | E.t u: (:+:) infixl 0 t u & print t & print u

class print t where
	print :: CPPState t -> String

	printp :: CPPState t -> String | print t
	printp st x :== if st.cpp_parens ("(" +++ print st x +++ ")") (print {st & cpp_parens=True} x)

class join e where
	join :: CPPState t e -> String | print t

	join_start :: CPPState t e -> String | print t
	join_start st glue elems :== if (isEmpty elems) "" (print st glue) +++ join st glue elems

instance zero CPPState

instance print String, Int, [t] | print t, CPPState, PrintList

instance join [u] | print u
