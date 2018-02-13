.PHONY: default build install uninstall

default: build

build:
	jbuilder build @install

install:
	jbuilder install statsd-client

uninstall:
	jbuilder uninstall

.PHONY: clean
clean:
	jbuilder clean
