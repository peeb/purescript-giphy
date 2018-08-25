.DEFAULT_GOAL := build

.PHONY: = build clean install optimise psc test

HTML = dist/index.html

JS = dist/js/bundle.js

install: psc

build: install
	npx pulp browserify --optimise --to $(JS)

clean:
	npx rimraf dist/js output

open:
	npx opener $(HTML)

optimise: build
	npx uglify-js $(JS) --output=$(JS)

psc:
	npx psc-package install

test:
	npx pulp test
