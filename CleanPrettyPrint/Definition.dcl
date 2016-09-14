definition module CleanPrettyPrint.Definition

from CleanPrettyPrint.Util import class print

from syntax import :: ParsedDefinition, :: Type, :: AType, :: ATypeVar, :: TypeContext

instance print ParsedDefinition, Type, AType, ATypeVar, TypeContext
