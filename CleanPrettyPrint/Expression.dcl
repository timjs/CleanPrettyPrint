definition module CleanPrettyPrint.Expression

from CleanPrettyPrint.Util import class print

from syntax import :: ParsedExpr, :: Rhs, :: OptGuardedAlts

instance print ParsedExpr, Rhs

compound_rhs :: OptGuardedAlts -> Bool // True iff no guards or #-lets
