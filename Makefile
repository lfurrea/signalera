ERL= $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR= $(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

DEPSOLVER_PLT= $(CURDIR)/.depsolver_plt

$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
	--apps erts kernel stdlib crypto public_key -r deps

.PHONY: dialyzer typer clean distclean

all: compile

compile: $(REBAR) get-deps compile

test: $(REBAR) eunit

dialyzer:$(DEPSOLVER_PLT)
	dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src

typer:$(DEPSOLVER_PLT)
	typer --plt $(DEPSOLVER_PLT) -r ./src

clean: $(REBAR) clean 

distclean: clean
	   rm -rf erl_crash.dump *~ *#
	   rm $(DEPSOLVER_PLT)
	   rm -rvf $(CURDIR)/deps/*
	   rm -rvf $(CURDIR)/doc/*