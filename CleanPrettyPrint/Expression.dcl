definition module CleanPrettyPrint.Expression

from CleanPrettyPrint.Util import class print

from syntax import :: ParsedExpr, :: Rhs

instance print ParsedExpr, Rhs
