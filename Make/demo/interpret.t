========Basic functionality tests======

Simple targets and recipes
  $ cd basic_functionality/targets_recipes
  $ diff <(make) <(demoInterpret)
  $ diff <(make a) <(demoInterpret a)
  $ diff <(make b) <(demoInterpret b)

Timestamp checking
  $ cd ../timestamps
  $ make clean
  rm go go.o primes.o
  rm: cannot remove 'go': No such file or directory
  rm: cannot remove 'go.o': No such file or directory
  rm: cannot remove 'primes.o': No such file or directory
  make: *** [Makefile:15: clean] Error 1
  [2]
  $ make >> o1
  $ touch primes.h
  $ make >> o1
  $ demoInterpret clean >> o2
  $ demoInterpret >> o2
  $ touch primes.h
  $ demoInterpret >> o2
  $ diff o1 o2
  0a1
  > rm go go.o primes.o
  [1]

  $ make clean >> o1
  $ make >> o1
  $ touch go.c
  $ make >> o1
  $ demoInterpret clean >> o2
  $ demoInterpret >> o2
  $ touch go.c
  $ demoInterpret >> o2
  $ diff o1 o2
  0a1
  > rm go go.o primes.o
  [1]

  $ make clean >> o1
  $ make >> o1
  $ touch primes.o
  $ make >> o1
  $ demoInterpret clean >> o2
  $ demoInterpret >> o2
  $ touch primes.o
  $ demoInterpret >> o2
  $ diff o1 o2
  0a1
  > rm go go.o primes.o
  [1]

Circular dependencies dropping:
  $ cd ../dropping
  $ diff <(make) <(demoInterpret)
  make: Circular d <- b dependency dropped.
  make: Circular f <- a dependency dropped.
  make: Circular f <- c dependency dropped.
  make: Circular f <- d dependency dropped.
  0a1,4
  > make: Circular d <- b dependency dropped.
  > make: Circular f <- a dependency dropped.
  > make: Circular f <- c dependency dropped.
  > make: Circular f <- d dependency dropped.
  [1]

Old recipes overriding:
  $ cd ../overriding
  $ diff <(make) <(demoInterpret)
  Makefile:5: warning: overriding recipe for target 'a'
  Makefile:3: warning: ignoring old recipe for target 'a'
  Makefile:7: warning: overriding recipe for target 'a'
  Makefile:5: warning: ignoring old recipe for target 'a'
  Makefile:9: warning: overriding recipe for target 'a'
  Makefile:7: warning: ignoring old recipe for target 'a'
  Makefile:12: warning: overriding recipe for target 'x'
  Makefile:7: warning: ignoring old recipe for target 'x'
  Makefile:12: warning: overriding recipe for target 'b'
  Makefile:9: warning: ignoring old recipe for target 'b'
  Makefile: warning: overriding recipe for target 'a'
   Makefile: warning: ignoring old recipe for target 'a'
  Makefile: warning: overriding recipe for target 'a'
   Makefile: warning: ignoring old recipe for target 'a'
  Makefile: warning: overriding recipe for target 'a'
   Makefile: warning: ignoring old recipe for target 'a'
  Makefile: warning: overriding recipe for target 'x'
   Makefile: warning: ignoring old recipe for target 'x'
  Makefile: warning: overriding recipe for target 'b'
   Makefile: warning: ignoring old recipe for target 'b'

We do not backtrack nodes when performing DFS, cause
that is either not possible with Graphlib, or very inefficient
  $ cd ../targets_dependencies
  $ diff <(make) <(demoInterpret)
  make: *** No rule to make target 'c', needed by 'a'.  Stop.
  10a11
  > make: *** No rule to make target 'c'. Stop.
  [1]
  $ diff <(make a) <(demoInterpret a)
  make: *** No rule to make target 'c', needed by 'a'.  Stop.
  10a11
  > make: *** No rule to make target 'c'. Stop.
  [1]
  $ diff <(make b) <(demoInterpret b)
  $ diff <(make c) <(demoInterpret c)
  make: *** No rule to make target 'c'.  Stop.
  0a1
  > make: Nothing to be done for 'c'.
  [1]
  $ diff <(make d) <(demoInterpret d)
  $ diff <(make x) <(demoInterpret x)
  $ diff <(make y) <(demoInterpret y)

Comments
  $ cd ../comments
  $ diff <(make) <(demoInterpret)
  $ diff <(make a) <(demoInterpret a)
  $ diff <(make b) <(demoInterpret b)

Tabs and Spaces
  $ cd ../empty_symbols
  $ diff <(make) <(demoInterpret)
  1c1
  < echo a
  ---
  > 							echo a
  2a3,17
  >  	 	   
  > 		   	
  > 				 
  >  
  >  	 
  > 	 	
  > 			  	
  > 
  > 	
  >  		 				 	
  > 							 	
  > 								 	
  > 										 
  > 										 	
  > 											 
  [1]
  $ diff <(make a) <(demoInterpret a)
  1c1
  < echo a
  ---
  > 							echo a
  2a3,17
  >  	 	   
  > 		   	
  > 				 
  >  
  >  	 
  > 	 	
  > 			  	
  > 
  > 	
  >  		 				 	
  > 							 	
  > 								 	
  > 										 
  > 										 	
  > 											 
  [1]
  $ diff <(make b) <(demoInterpret b)

Multiline tests
We are expanding multiline recipes to one line, make dont.
  $ cd ../multiline
  $ diff <(make) <(demoInterpret)
  1,2c1
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdf
  4,5c3
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdf
  7,8c5
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdf
  10,15c7
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf\
  < sfsdf\
  < sfsd\
  < fsdfdssd\
  < fsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdfsfsdfsfsdfsdfdssdfsdf
  [1]
  $ diff <(make a) <(demoInterpret a)
  1,2c1
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdf
  4,5c3
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdf
  7,8c5
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdf
  10,15c7
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf\
  < sfsdf\
  < sfsd\
  < fsdfdssd\
  < fsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdfsfsdfsfsdfsdfdssdfsdf
  [1]
  $ diff <(make b) <(demoInterpret b)
  1,2c1
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdf
  [1]

Echoing
  $ cd ../echoing
  $ diff <(make a) <(demoInterpret a)
  $ diff <(make b) <(demoInterpret b)

Specifying recipe on the same line with targets
  $ cd ../recipe_on_same_line
  $ diff <(make a) <(demoInterpret a)
  $ diff <(make x) <(demoInterpret x)

Advanced functionality
There is a difference, but that caused by the fact that we could DFS-traverse
our graph in a different ways. You can see that, in fact, there are no difference
if we sort the output
  $ cd ../../advanced_functionality/variables
  $ diff <(make) <(demoInterpret)
  make: Circular 4 <- 4 dependency dropped.
  make: Circular 5 <- 5 dependency dropped.
  make: Circular 6 <- 6 dependency dropped.
  0a1,6
  > 1
  > 1
  > 1
  > 4
  > 5
  > 6
  4d9
  < d
  5a11,12
  > true
  > d
  8d14
  < z
  12,18c18,22
  < true
  < 1
  < 2
  < 3
  < 
  < 
  < 
  ---
  > z
  > make: Circular 4 <- 4 dependency dropped.
  > make: Circular 5 <- 5 dependency dropped.
  > make: Circular 6 <- 6 dependency dropped.
  > make: Nothing to be done for 'all'.
  [1]
  $ diff <(make) <(demoInterpret)
  make: Circular 4 <- 4 dependency dropped.
  make: Circular 5 <- 5 dependency dropped.
  make: Circular 6 <- 6 dependency dropped.
  0a1,6
  > 1
  > 1
  > 1
  > 4
  > 5
  > 6
  4d9
  < d
  5a11,12
  > true
  > d
  8d14
  < z
  12,18c18,22
  < true
  < 1
  < 2
  < 3
  < 
  < 
  < 
  ---
  > z
  > make: Circular 4 <- 4 dependency dropped.
  > make: Circular 5 <- 5 dependency dropped.
  > make: Circular 6 <- 6 dependency dropped.
  > make: Nothing to be done for 'all'.
  [1]

A lot of substitutions
  $ cd ../subsitution_madness
  $ diff <(make) <(demoInterpret)
  /bin/sh: line 1: o: command not found
  make: [Makefile:10: allcffc] Error 127 (ignored)
  sh: line 1: -o: command not found
  3c3,4
  < o c.o -Wall allcffc && echo allcffc ccc
  ---
  >  -o c.o -Wall allcffc && echo allcffc ccc
  > make: *** [Makefile: 'allcffc'] Error 127
  [1]

Real-world example. Showcase of a pattern usage (after the first make,
we match %.o files since they are already exist and that leads to the
recipe overriding).
Basic make doesnt show much info about overriding, but we show all of it.
  $ cd ../real_life
  $ mkdir obj
  $ diff <(make) <(demoInterpret)
  Makefile: warning: overriding recipe for target 'obj/args_check.o'
   Makefile: warning: ignoring old recipe for target 'obj/args_check.o'
  Makefile: warning: overriding recipe for target 'obj/connection.o'
   Makefile: warning: ignoring old recipe for target 'obj/connection.o'
  Makefile: warning: overriding recipe for target 'obj/dump_wifi_params.o'
   Makefile: warning: ignoring old recipe for target 'obj/dump_wifi_params.o'
  1,7c1
  < Matched obj/args_check
  < Compiled src/args_check.c successfully!
  < Matched obj/connection
  < Compiled src/connection.c successfully!
  < Matched obj/dump_wifi_params
  < Compiled src/dump_wifi_params.c successfully!
  < Matched obj/telnet_remote_control
  ---
  > Matched 
  9c3
  < Matched obj/tftp_server
  ---
  > Matched 
  [1]
  $ touch src/args_check.c
  $ make >> o1
  $ touch src/args_check.c
  $ demoInterpret >> o2
  Makefile: warning: overriding recipe for target 'obj/args_check.o'
   Makefile: warning: ignoring old recipe for target 'obj/args_check.o'
  Makefile: warning: overriding recipe for target 'obj/connection.o'
   Makefile: warning: ignoring old recipe for target 'obj/connection.o'
  Makefile: warning: overriding recipe for target 'obj/dump_wifi_params.o'
   Makefile: warning: ignoring old recipe for target 'obj/dump_wifi_params.o'
  Makefile: warning: overriding recipe for target 'obj/telnet_remote_control.o'
   Makefile: warning: ignoring old recipe for target 'obj/telnet_remote_control.o'
  Makefile: warning: overriding recipe for target 'obj/tftp_server.o'
   Makefile: warning: ignoring old recipe for target 'obj/tftp_server.o'
  $ diff o1 o2
  1c1
  < Matched obj/args_check
  ---
  > Matched args_check
  [1]


