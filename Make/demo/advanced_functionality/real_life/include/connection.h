/** @file
 * @brief Wrapper for bind and connect functions.
 *
 * Copyright (C) 2020 OKTET Labs, St.-Petersburg, Russia
 *
 * @author Ivan Morozko <Ivan.Morozko@oktetlabs.ru>
 *
 * $Id: $
 */

#ifndef _CONNECTION_
#define _CONNECTION_

#include <stdbool.h>
#include <stdint.h>

typedef struct conn_info {
    int               sock;
    int               port;
    const char        *addr;
} conn_info;

extern int socket_bind(conn_info *info);

extern int socket_connect(conn_info *info);

extern int conn_info_fill(conn_info *ret, const char *ip_addr,
                          const int port, int sock_type);

/** Get sock field from 'info' */
static inline int get_sock(conn_info *info)
{
    return info->sock;
}

/** Get port field from 'info' */
static inline int get_port(conn_info *info)
{
    return info->port;
}

/** Get addr field from 'info' */
static inline const char * get_addr(conn_info *info)
{
    return info->addr;
}

#endif
