.PHONY: all test clean

SWIPL := swipl

server:
	@$(SWIPL) -g main server.pl
