implementation module CleanPrettyPrint

import StdEnv

import syntax

import CleanPrettyPrint.Util
import CleanPrettyPrint.Common
import CleanPrettyPrint.Expression
import CleanPrettyPrint.Definition

instance cpp ParsedDefinition where cpp pd = print zero pd
