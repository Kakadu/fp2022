/** @file
 * @brief Arguments checking for dump_wifi_params tool.
 *
 * Copyright (C) 2020 OKTET Labs, St.-Petersburg, Russia
 *
 * @author Ivan Morozko <Ivan.Morozko@oktetlabs.ru>
 *
 * $Id: $
 */

#ifndef _ARGS_CHECK
#define _ARGS_CHECK

#include "include/telnet_remote_control.h"
#include "include/tftp_server.h"

#define FLAG_QUIET               1

typedef struct dump_params_options {
    int                     flags;
    char                    *board_type;
    telnet_auth_options     telnet_opt;
    tftp_server_options     tftp_opt;
} dump_params_options;

extern int options_get(int argc, char **argv);

#endif
