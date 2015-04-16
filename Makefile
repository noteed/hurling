all: .hurling_touched

dist/build/hurling/hurling: \
  bin/hurling.hs
	./build.sh

images/hurling/hurling: dist/build/hurling/hurling
	cp $< $@

images/hurling/humming: ../humming/dist/build/humming/humming
	cp $< $@

.hurling_touched: images/hurling/Dockerfile images/hurling/hurling images/hurling/humming
	docker build -t noteed/hurling images/hurling
	touch .hurling_touched
