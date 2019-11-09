help:
	@printf "==================================================\n";
	@printf "             Wudao dict makefile\n";
	@printf "==================================================\n";
	@printf "\n";
	@printf "command:\n";
	@printf "\n";
	@printf "       - help: print this help\n";
	@printf "       - install: install wudao-dict\n";
	@printf "       - batch-query:\n";
	@printf "\n";
	@printf "         Batch query sequenced by =query.dict=, to generate the cache file:\n";
	@printf "         1. <query.cache>: the full translation cache\n";
	@printf "         2. <query.cache_desc>: same as above but for short version refer to 'wd -s'\n";
	@printf "\n";

install:
	@cd wudao-dict && bash setup.sh

uninstall:
	@cd wudao-dict && bash uninstall.sh

batch-query:
	@bash query-batch.sh
