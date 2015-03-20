all: deps compile

.dialyzer.plt:
		dialyzer --build_plt \
				--output_plt .dialyzer.plt \
				--apps kernel stdlib sasl erts ssl \
					tools os_mon runtime_tools crypto \
					inets xmerl webtool snmp public_key \
					mnesia eunit syntax_tools compiler
					# TODO ./include/*/ebin

dialyzer: .dialyzer.plt
		dialyzer ./ebin --plt .dialyzer.plt \
				-Wunmatched_returns \
				-Werror_handling \
				-Wrace_conditions \
				-Wbehaviours \
				-Wunderspecs
deps:
		rebar -C rebar.conf get-deps

compile:
		rebar -C rebar.conf compile

test:
		rebar -C rebar.conf skip_deps=true eunit

clean:
		rebar -C rebar.conf clean

local:
		HOSTNAME=`hostname -s`
		for i in {a..e}; do erl -pa deps/lager/ebin -pa ebin -sname $$i -connect_all false -seeds a@$$HOSTNAME -noinput & done
