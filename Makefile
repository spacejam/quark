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
		rebar get-deps

compile:
		rebar compile

test:
		rebar skip_deps=true eunit

clean:
		rebar clean
