# Copyright (c) 2009-2013, Dmitry Vasiliev <dima@hlabs.org>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#  * Neither the name of the copyright holders nor the names of its
#    contributors may be used to endorse or promote products derived from this
#    software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

RELDIR = ebin
TESTDIR = .eunit

HEADERS = $(wildcard src/*.hrl)
SOURCES = $(wildcard src/*.erl)
BEAMS = $(patsubst src/%.erl,$(RELDIR)/%.beam,$(SOURCES))
TESTSOURCES = $(wildcard test/*.erl)
TESTBEAMS = $(patsubst src/%.erl,$(TESTDIR)/%.beam,$(SOURCES)) \
    $(patsubst test/%.erl,$(TESTDIR)/%.beam,$(TESTSOURCES))
ERLC = erlc -Wall +warnings_as_errors -I include -I src
ERL = erl -noinput -pa ../erlport


compile: $(BEAMS)

compile-test: $(TESTDIR) $(TESTDIR)/erlport.app $(TESTBEAMS)

$(RELDIR)/%.beam: src/%.erl $(HEADERS)
	$(ERLC) +debug_info -o $(RELDIR) $<

$(TESTDIR)/%.beam: test/%.erl
	$(ERLC) +debug_info -o $(TESTDIR) $<

$(TESTDIR)/%.beam: src/%.erl $(HEADERS)
	$(ERLC) +debug_info -o $(TESTDIR) $<

$(TESTDIR):
	mkdir $@

$(TESTDIR)/erlport.app:
	cp -l ebin/erlport.app $(TESTDIR)

test: python-test ruby-test erlang-test

test-verbose: python-test-verbose ruby-test erlang-test-verbose

erlang-test: compile-test
	./runtest

erlang-test-verbose: compile-test
	./runtest verbose

ruby-test: ruby1.8-test ruby1.9-test

ruby1.8-test:
	cd priv/ruby1.8; make test

ruby1.9-test:
	cd priv/ruby1.9; make test

python-test: python2-test python3-test

python-test-verbose: python2-test-verbose python3-test-verbose

python2-test:
	cd priv/python2; make test

python2-test-verbose:
	cd priv/python2; make test-verbose

python3-test:
	cd priv/python3; make test

python3-test-verbose:
	cd priv/python3; make test-verbose

check: $(TESTDIR) $(TESTBEAMS)
	dialyzer $(TESTDIR) | fgrep -v -f dialyzer.ignore

create-ignore-file: $(TESTDIR) $(TESTBEAMS)
	dialyzer $(TESTDIR) | egrep '^[^:]+:[0-9]+: ' > dialyzer.ignore; exit 0

doc:
	$(ERL) -eval 'edoc:application(erlport)' -s init stop

clean: erlang-clean python-clean ruby-clean doc-clean

doc-clean:
	rm -f doc/*.html doc/*.png doc/*.css doc/edoc-info

erlang-clean:
	rm -rf $(RELDIR)/*.beam $(TESTDIR)
	find test \( -name '*.py[co]' -o -name '__pycache__' \) -delete

python-clean: python2-clean python3-clean

python2-clean:
	cd priv/python2; make clean

python3-clean:
	cd priv/python3; make clean

ruby-clean: ruby1.8-clean ruby1.9-clean

ruby1.8-clean:
	cd priv/ruby1.8; make clean

ruby1.9-clean:
	cd priv/ruby1.9; make clean

release: clean compile
	./release bin

release-src: clean
	./release src


.PHONY: compile compile-test test test-verbose check doc clean python2-test
.PHONY: python2-test-verbose create-ignore-file python3-test
.PHONY: python3-test-verbose python-test python-test-verbose
.PHONY: erlang-test erlang-test-verbose
.PHONY: ruby-test ruby1.8-test ruby1.9-test
.PHONY: erlang-clean python-clean python2-clean python3-clean
.PHONY: ruby-clean ruby1.8-clean ruby1.9-clean
.PHONY: release release-src
