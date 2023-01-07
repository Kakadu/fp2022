ALL_TARGETS = a b c d cd e f z l what_is_going_on will_not_change check 1 2 3 4 5 6

all: $(ALL_TARGETS)

a:;@echo a #normal way of printing X when making target X

$(b)b:; @echo $(b)$($(b)b)b #since variable "b" is not defined yet, it expands to an empty string

c = c
$($($($($($($($(c)))))))):; @echo $($($(c))) #it is possible to recursively expand a variable

d=smth else
d = d
$(d):; @echo $(d) # We're always using the most recent definition

$(c)$(d) = $(c)$(d) # it is possible to subsititute variables while defining another variable
# This WONT produce the desired result, instead, we'll print "smth else". Why? See below:
$($(c)$(d)): ; @echo $(cd)
cd = smth else
# Targets and prerequisites are expanded immediately, however, recipes are expanded right before an actual execution
# (One could note that we expanded $(cd) in the targets section to "cd", not to "smth else".)

# ------ Recursively Expanded Variables, `=` ------
#
# When defining variable witn =, if it value contains subsitutions, it doesn't expand immediately
# We expand it when we do an actual subsitution next time
e: ; @echo $(foo)
foo = $(bar)
bar = $(e)
e = e

# ------ Simply Expanded Variables, `:=` ------
#
# When defining variable witn :=, if it value contains subsitutions, we perform them immediately and.
f:; @echo $(f) #here f is just "f", cause "useless_var" expanded immediately to the empty string
f := $(useless_var)f
useless_var = i'm sad

# ------ Conditional Expanded Variables, `?=` ------
# It only has an effect if the variable is not yet defined.
# Other than that, it performs as a recursive expanded variable
will_not_change ?= true
will_not_change: ; @echo $(will_not_change)
will_not_change ?= false
check: ; @echo $(will_not_change)

# ----- Interaction with targets/prereqisites -----
# The difference between simply and recursively expanded variables only happens when we're substituting in recipes.

x = $(y) # here "x" is undecided, cause of recursive assigment
$(x)z:;@echo $(x) #immediately expand x in targets, but not in recipes
y = z # make z will print z

p := $(q) #p is an empty string, cause of simple assigment
$(p)l :; @echo $(p)l
q = l #make l will pring l (NOT ll), cause of simple assigment

# ----- Some strange cases when defining variable -----

# Everything expands when defining a name for a variable
is = what_is_
$(what)$(is)going_on = $(what)
what = magic
$(is)going_on: ;@echo $($(is)going_on)

zzz = a b
t = $($(zzz))
test: ;@echo $(t)

targets = 1 2 3
$(targets): ; @echo $@ # $@ expands to the current target name
4:4 ; @echo $< # $< expands to the first prerequisite
5:5 ; @echo $< # $< expands to the first prerequisite
6:6 ; @echo $< # $< expands to the first prerequisite
