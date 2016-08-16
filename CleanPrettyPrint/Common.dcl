definition module CleanPrettyPrint.Common

from CleanPrettyPrint.Util import class print

from syntax import :: Ident, :: Import

instance print Ident, Import
