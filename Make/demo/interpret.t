========Basic functionality tests======

Simple targets and recipes
  $ cd basic_functionality/targets_recipes
  $ make > f1
  $ demoInterpret > f2
  $ diff f1 f2
  $ make a > f1
  $ demoInterpret a > f2
  $ diff f1 f2
  $ make b > f1
  $ demoInterpret b > f2
  $ diff f1 f2

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
  $ make > f1
  make: Circular d <- b dependency dropped.
  make: Circular f <- a dependency dropped.
  make: Circular f <- c dependency dropped.
  make: Circular f <- d dependency dropped.
  $ demoInterpret > f2
  $ diff f1 f2
  0a1,4
  > make: Circular d <- b dependency dropped.
  > make: Circular f <- a dependency dropped.
  > make: Circular f <- c dependency dropped.
  > make: Circular f <- d dependency dropped.
  [1]

Old recipes overriding:
  $ cd ../overriding
  $ make > f1
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
  $ demoInterpret > f2
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
  $ diff f1 f2

We do not backtrack nodes when performing DFS, cause
that is either not possible with Graphlib, or very inefficient
  $ cd ../targets_dependencies
  $ make > f1
  make: *** No rule to make target 'c', needed by 'a'.  Stop.
  [2]
  $ demoInterpret > f2
  $ diff f1 f2
  10a11
  > make: *** No rule to make target 'c'. Stop.
  [1]
  $ make a > f1
  make: *** No rule to make target 'c', needed by 'a'.  Stop.
  [2]
  $ demoInterpret a > f2
  $ diff f1 f2
  10a11
  > make: *** No rule to make target 'c'. Stop.
  [1]
  $ make b > f1
  $ demoInterpret b > f2
  $ diff f1 f2
  $ make c > f1
  make: *** No rule to make target 'c'.  Stop.
  [2]
  $ demoInterpret c > f2
  $ diff f1 f2
  0a1
  > make: Nothing to be done for 'c'.
  [1]
  $ make d > f1
  $ demoInterpret d > f2
  $ diff f1 f2
  $ make x > f1
  $ demoInterpret x > f2
  $ diff f1 f2
  $ make y > f1
  $ demoInterpret y > f2
  $ diff f1 f2

Comments
  $ cd ../comments
  $ make > f1
  $ demoInterpret > f2
  $ diff f1 f2
  $ make a > f1
  $ demoInterpret a > f2
  $ diff f1 f2
  $ make b > f1
  $ demoInterpret b > f2
  $ diff f1 f2

Tabs and Spaces
  $ cd ../empty_symbols
  $ make > f1
  $ demoInterpret > f2
  $ diff f1 f2
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
  $ make a > f1
  $ demoInterpret a > f2
  $ diff f1 f2
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
  $ make b > f1
  $ demoInterpret b > f2
  $ diff f1 f2

Multiline tests
We are expanding multiline recipes to one line, make dont.
  $ cd ../multiline
  $ make > f1
  $ demoInterpret > f2
  $ diff f1 f2
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
  $ make a > f1
  $ demoInterpret a > f2
  $ diff f1 f2
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
  $ make b > f1
  $ demoInterpret b > f2
  $ diff f1 f2
  1,2c1
  < echo asfasdfasfsf\
  < fdsfdsfsdsdfsdf
  ---
  > echo asfasdfasfsffdsfdsfsdsdfsdf
  [1]

Echoing
  $ cd ../echoing
  $ make a > f1
  $ demoInterpret a > f2
  $ diff f1 f2
  $ make b > f1
  $ demoInterpret b > f2
  $ diff f1 f2

Specifying recipe on the same line with targets
  $ cd ../recipe_on_same_line
  $ make a > f1
  $ demoInterpret a > f2
  $ diff f1 f2
  $ make x > f1
  $ demoInterpret x > f2
  $ diff f1 f2

Advanced functionality
There is a difference, but that caused by the fact that we could DFS-traverse
our graph in a different ways. You can see that, in fact, there are no difference
if we sort the output
  $ cd ../../advanced_functionality/variables
  $ make > f1
  make: Circular 4 <- 4 dependency dropped.
  make: Circular 5 <- 5 dependency dropped.
  make: Circular 6 <- 6 dependency dropped.
  $ demoInterpret > f2
  $ diff f1 f2
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
  $ make > f1
  make: Circular 4 <- 4 dependency dropped.
  make: Circular 5 <- 5 dependency dropped.
  make: Circular 6 <- 6 dependency dropped.
  $ demoInterpret > f2
  $ diff f1 f2
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
  $ make > f1
  $ demoInterpret > f2
  $ diff f1 f2

Real-world example. Showcase of a pattern usage (after the first make,
we match %.o files since they are already exist and that leads to the
recipe overriding).
Basic make doesnt show much info about overriding, but we show all of it.
  $ cd ../real_life
  $ mkdir obj
  $ make > f1
  $ demoInterpret > f2
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
  $ diff f1 f2
  1,11c1
  < Matched obj/args_check
  < Compiled src/args_check.c successfully!
  < Matched obj/connection
  < Compiled src/connection.c successfully!
  < Matched obj/dump_wifi_params
  < Compiled src/dump_wifi_params.c successfully!
  < Matched obj/telnet_remote_control
  < Compiled src/telnet_remote_control.c successfully!
  < Matched obj/tftp_server
  < Compiled src/tftp_server.c successfully!
  < Linking complete!
  ---
  > make: 'dump_wifi_params' is up to date.
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


