definition module CleanPrettyPrint

from syntax import :: ParsedDefinition, :: ParsedExpr, :: Rhs, :: Type, :: TypeContext

class cpp t :: t -> String

instance cpp
	ParsedDefinition,
	ParsedExpr,
	Rhs,
	Type,
	TypeContext
