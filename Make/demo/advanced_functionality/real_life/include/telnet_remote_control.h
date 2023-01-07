/** @file
 * @brief Interface for executing commands via telnet.
 *
 * Functions for authorising and executing commands on
 * remote telnet server.
 *
 * Copyright (C) 2020 OKTET Labs, St.-Petersburg, Russia
 *
 * @author Ivan Morozko <Ivan.Morozko@oktetlabs.ru>
 *
 * $Id: $
 */

#ifndef _TELNET_REMOTE_CONTROL_
#define _TELNET_REMOTE_CONTROL_

#include "connection.h"

typedef struct telnet_auth_options {
    const char            *addr;
    const char            *port;
    const char            *username;
    const char            *password;
    const char            *login_prompt;
    /* The string we are waiting from the server, so we can enter username. */
    const char            *password_prompt;
    /* The string we are waiting from the server, so we can enter password. */
    const char            *cl_prompt;
    /* The string that server would send as command line prompt.            */
} telnet_auth_options;

typedef struct telnet_cmd_data {
    const char            *command;
    const char            *error_substr;
    /* The substring that would be in server responce if an error occures.  */
} telnet_cmd_data;

typedef struct telnet_board_data {
    conn_info             tcp_conn;
    telnet_auth_options   *opt;
} telnet_board_data;

extern int telnet_fill_board_data(telnet_board_data *ret,
                                  telnet_auth_options *opt);

extern int telnet_free_board_data(telnet_board_data *data);

extern int telnet_auth(telnet_board_data *data);

extern int telnet_execute_command(telnet_board_data *data,
                                  telnet_cmd_data *cmd_data);
#endif
