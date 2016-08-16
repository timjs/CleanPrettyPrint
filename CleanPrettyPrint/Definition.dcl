definition module CleanPrettyPrint.Definition

from CleanPrettyPrint.Util import class print

from syntax import :: ParsedDefinition, :: Type, :: TypeContext

instance print ParsedDefinition, Type, TypeContext
