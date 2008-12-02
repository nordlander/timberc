TIMBERC=timberc

EXAMPLES = Echo Echo2 Echo3 EchoServer EchoServer2 MasterMind Primes Reflex TCPClient PingTimeServers

all: 
	@for ex in $(EXAMPLES); do \
		$(TIMBERC) --make  $$ex; \
	done
