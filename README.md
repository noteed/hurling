# Hurling - a Humming worker running Docker commands

The idea of Hurlign is to read `docker run` commands from a Humming queue and
actually call `docker run`.

A special case where a GitHub Hooks `push` payload is implemented and result in
a call to `images.reesd.com/reesd/builder`.

A sample Docker image used to try Hurling can be built with `make` and run
with:

    > docker run -v /var/run/docker.sock:/var/run/docker.sock noteed/hurling
