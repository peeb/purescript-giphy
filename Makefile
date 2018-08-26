.DEFAULT_GOAL := build

.PHONY: = build clean install optimise psc test update

HTML_OUTPUT = dist/index.html

JS_OUTPUT = dist/js/bundle.js

install: psc

build: install
	npx pulp browserify --optimise --to $(JS_OUTPUT)

clean:
	npx rimraf dist/js output

open:
	npx opener $(HTML_OUTPUT)

optimise: build
	npx uglify-js $(JS_OUTPUT) --output=$(JS_OUTPUT)

psc:
	npx psc-package install

test:
	npx pulp test

update:
	npx psc-package updates
