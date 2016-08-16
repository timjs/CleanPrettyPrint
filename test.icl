module test

import StdArray
import StdFile
import ArgEnv

// frontend
import Heap
import compile
import parse
import predef

import CleanPrettyPrint

clean_home :== "/opt/clean"
default_lib :== "clean-platform/OS-Independent"
default_mod_id :== "Data.Func"
dcl :== False

mkdir :: String -> String
mkdir s = toString (map (\c.case c of '.'='/'; c=c) (fromString s))

Start :: *World -> *World
Start w
# args = [a \\ a <-: getCommandLine]
# mod_id = if (length args > 1) (args!!1) default_mod_id
# lib = if (length args > 2) (args!!2) default_lib
# filename = clean_home +++ "/lib/" +++ lib +++ "/" +++ mkdir mod_id +++ if dcl ".dcl" ".icl"
# (ok,f,w) = fopen filename FReadText w
| not ok = abort ("Couldn't open file " +++ filename +++ "\n")
# (st, w) = init_identifiers newHeap w
# cache = empty_cache st
# (mod_id, ht) = putIdentInHashTable mod_id (IC_Module NoQualifiedIdents) cache.hash_table
  cache = {cache & hash_table=ht}
# ((b1,b2,pm,ht,f),w) = accFiles (wantModule` f "" (not dcl) mod_id.boxed_ident NoPos True ht stderr) w
# (ok,w) = fclose f w
# pds = pm.mod_defs
# (io,w) = stdio w
# io = io <<< (join "\n" (map cpp pds) +++ "\n")
# (_,w) = fclose io w
= w

wantModule` :: !*File !{#Char} !Bool !Ident !Position !Bool !*HashTable !*File !*Files
	-> ((!Bool,!Bool,!ParsedModule, !*HashTable, !*File), !*Files)
wantModule` f s b1 i p b2 ht io fs
# (b1,b2,pm,ht,f,fs) = wantModule f s b1 i p b2 ht io fs
= ((b1,b2,pm,ht,f),fs)

join :: a [b] -> String | toString a & toString b
join _ []     = ""
join _ [x]    = toString x
join g [x:xs] = toString x +++ toString g +++ join g xs
