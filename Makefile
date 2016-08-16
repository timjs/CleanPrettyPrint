COCL?=/path/to/clean-compiler/branches/itasks

CLM=clm
CLMFLAGS=\
		 -tst -ms\
		 -I $$CLEAN_HOME/lib/StdLib\
		 -I $$CLEAN_HOME/lib/ArgEnv\
		 -I $(COCL)/frontend\
		 -I $(COCL)/main\
		 -I $(COCL)/main/Unix\
		 -I $(COCL)/backend\
		 -nr -nt

test: %: %.icl $(wildcard *.icl) $(wildcard *.dcl) $(wildcard **/*.icl) $(wildcard **/*.dcl)
	$(CLM) $(CLMFLAGS) $@ -o $@

clean:
	$(RM) -vr test Clean\ System\ Files
