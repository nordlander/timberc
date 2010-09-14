TIMBERC=timberc

EXAMPLES = Echo Echo2 Echo3 EchoServer EchoServer2 MasterMind Primes Reflex TCPClient PingTimeServers SantaProblem2

BROWSER_EXAMPLES = Scribbler

examples: 
	@for ex in $(EXAMPLES); do \
		$(TIMBERC) --make  $$ex; \
	done

	@for ex in $(BROWSER_EXAMPLES); do \
		$(TIMBERC) --target Browser --make  $$ex; \
	done

local: 
	@for ex in $(EXAMPLES); do \
		../timberc --make  $$ex; \
	done
	@for ex in $(BROWSER_EXAMPLES); do \
		../timberc --target Browser --make  $$ex; \
	done

