implementation module CleanPrettyPrint

import StdEnv

import syntax

import CleanPrettyPrint.Util
import CleanPrettyPrint.Common
import CleanPrettyPrint.Expression
import CleanPrettyPrint.Definition

instance cpp ParsedDefinition where cpp x = print zero x
instance cpp ParsedExpr       where cpp x = print zero x
instance cpp Rhs              where cpp x = print zero x
instance cpp Type             where cpp x = print zero x
instance cpp TypeContext      where cpp x = print zero x
