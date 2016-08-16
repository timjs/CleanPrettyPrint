definition module CleanPrettyPrint

from syntax import :: ParsedDefinition

class cpp t :: t -> String

instance cpp ParsedDefinition
