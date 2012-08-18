# Copyright (c) 2009-2012, Dmitry Vasiliev <dima@hlabs.org>
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

SOURCES = $(wildcard src/*.erl)
BEAMS = $(patsubst src/%.erl,$(RELDIR)/%.beam,$(SOURCES))
TESTSOURCES = $(wildcard test/*.erl)
TESTBEAMS = $(patsubst src/%.erl,$(TESTDIR)/%.beam,$(SOURCES)) \
    $(patsubst test/%.erl,$(TESTDIR)/%.beam,$(TESTSOURCES))
ERLC = erlc -Wall +warnings_as_errors -I include
ERL = erl -noinput -pa ../erlport
 
 
compile: $(BEAMS)
 
$(RELDIR)/%.beam: src/%.erl
	$(ERLC) -o $(RELDIR) $<
 
$(TESTDIR)/%.beam: test/%.erl
	$(ERLC) +debug_info -o $(TESTDIR) $<
 
$(TESTDIR)/%.beam: src/%.erl
	$(ERLC) +debug_info -o $(TESTDIR) $<

$(TESTDIR):
	mkdir $@

$(TESTDIR)/erlport.app:
	cp -l ebin/erlport.app $(TESTDIR)

test: python-test $(TESTDIR) $(TESTDIR)/erlport.app $(TESTBEAMS)
	./runtest

test-verbose: python-test-verbose $(TESTDIR) $(TESTDIR)/erlport.app $(TESTBEAMS)
	./runtest verbose

python-test:
	cd priv/python; make test

python-test-verbose:
	cd priv/python; make test-verbose

check: $(TESTDIR) $(TESTBEAMS)
	dialyzer $(TESTDIR) | fgrep -v -f ./dialyzer.ignore

doc:
	$(ERL) -eval 'edoc:application(erlport)' -s init stop

clean:
	rm -rf $(RELDIR)/*.beam $(TESTDIR)


.PHONY: compile test test-verbose check doc clean python-test
.PHONY: python-test-verbose
