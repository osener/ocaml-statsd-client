.PHONY: default build install uninstall

default: build

build:
	jbuilder build @install

install: build
	jbuilder install statsd-client
	jbuilder install statsd-client-async
	jbuilder install statsd-client-lwt

uninstall:
	jbuilder uninstall

.PHONY: clean
clean:
	jbuilder clean
