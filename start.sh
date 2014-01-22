#!/bin/sh
erl -pa ebin deps/*/ebin -config game_server.config -s game_server \
  -eval "io:format(\"Game Server successfully started!~n\")."
