SRCDIR = src
OBJDIR = ebin

SRC = $(wildcard $(SRCDIR)/*.erl)
OBJ = $(patsubst $(SRCDIR)%,$(OBJDIR)%,$(patsubst %.erl, %.beam, $(SRC)))

ERLC        = erlc
ERLCFLAGS   = -v -W2 -Werror

ERL         = erl
ERLFLAGS    = -s init stop -noshell
MODULE      = tarry
# =================

.PHONY: default all $(MODULE)
all: $(MODULE)

$(MODULE): $(OBJ)

$(OBJDIR)/%.beam: $(SRCDIR)/%.erl | $(OBJDIR)
	$(ERLC) $(ERLCFLAGS) -o $(dir $@) $<

$(OBJDIR):
	@mkdir -p $@

.PHONY: run
run: $(MODULE)
	$(ERL) -pa $(OBJDIR) -run $(MODULE) $(ERLFLAGS)

.PHONY: clean
clean:
	$(RM) -r $(OBJDIR)
