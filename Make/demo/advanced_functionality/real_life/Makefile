TARGET   = dump_wifi_params

CC       = gcc
CFLAGS   = -Wall -Wextra -I.

LINKER   = gcc
LFLAGS   = -Wall -I. -lm

SRCDIR   = src
INCDIR   = include
OBJDIR   = obj

SOURCES  := $(SRCDIR)/args_check.c $(SRCDIR)/connection.c $(SRCDIR)/dump_wifi_params.c $(SRCDIR)/telnet_remote_control.c $(SRCDIR)/tftp_server.c
INCLUDES := $(INCDIR)/args_check.h $(INCDIR)/connection.h $(INCDIR)/telnet_remote_control.h $(INCDIR)/tftp_server.h
OBJECTS  := $(OBJDIR)/args_check.o $(OBJDIR)/connection.o $(OBJDIR)/dump_wifi_params.o $(OBJDIR)/telnet_remote_control.o $(OBJDIR)/tftp_server.o
rm       = rm -f

$(TARGET): $(OBJECTS)
	@$(LINKER) $(OBJECTS) $(LFLAGS) -o $@
	@echo "Linking complete!"

obj/args_check.o : src/args_check.c
	@echo "Matched "$*
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"
obj/connection.o : src/connection.c
	@echo "Matched "$*
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"
obj/dump_wifi_params.o : src/dump_wifi_params.c
	@echo "Matched "$*
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"
obj/telnet_remote_control.o : src/telnet_remote_control.c
	@echo "Matched "$*
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"
obj/tftp_server.o : src/tftp_server.c
	@echo "Matched "$*
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"

#We could also do this like so (it will override previous rules)
$(OBJDIR)/%.o: $(SRCDIR)/%.c
	@echo "Matched "$*
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"


clean:
	@$(rm) $(OBJECTS)
	@echo "Cleanup complete!"
