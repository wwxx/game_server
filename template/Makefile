## The MIT License (MIT)
##
## Copyright (c) 2014-2024
## Savin Max <mafei.198@gmail.com>
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

all:
	cd game_server && tools/install

test:
	cd game_server && tools/run_test

console:
	cd game_server && tools/start

start_development:
	cd game_server && tools/start_daemon -e development

start_production:
	cd game_server && tools/start_daemon -e production

stop_daemon:
	cd game_server && tools/stop_daemon

attach_daemon:
	cd game_server && tools/attach_daemon

clean:
	cd game_server && ./rebar clean

compile:
	cd game_server && ./rebar compile

rc:
	cd app/server_engine && rails c

setup_production:
	cd app/server_engine && RAILS_ENV=production bundle exec rake setup
resetup_production:
	cd game_server && ./rebar clean
	cd game_server && rm -rf Mnesia.*
	cd app/server_engine && bundle exec rake db:drop:all
	cd game_server && tools/install -e production

setup_development:
	cd app/server_engine && bundle exec rake setup
resetup_development:
	cd game_server && ./rebar clean
	cd game_server && rm -rf Mnesia.*
	cd app/server_engine && bundle exec rake db:drop:all
	cd game_server && tools/install
