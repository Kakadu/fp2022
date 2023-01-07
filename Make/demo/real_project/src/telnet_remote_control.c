#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <unistd.h>

#include "include/telnet_remote_control.h"

#define MAX_RECV_BUFF_SIZE  10000
#define TIMEOUT             3

/**
 * Fill 'ret' structure with appropriate data.
 *
 * @return
 *      Zero on success, or -1, if error occured.
 */
int telnet_fill_board_data(telnet_board_data *ret, telnet_auth_options *opt)
{
    int retval;

    retval = conn_info_fill(&ret->tcp_conn, opt->addr,
                            atoi(opt->port), SOCK_STREAM);
    if (retval)
        return retval;

    ret->opt = opt;

    return retval;
}

/**
 * Close socket for telnet_board_data.
 *
 * @return
 *      Zero on success, or -1, if error occured.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
int telnet_free_board_data(telnet_board_data *data)
{
    int retval;
    int s;

    s = get_sock(&data->tcp_conn);

    retval = close(s);
    if (retval)
        perror("telnet: close()");

    return retval;
}

/**
 * Wait for TIMEOUT seconds until server sends data that contains
 * 'expected' as substring.
 *
 * @return
 *      Zero on success, or -1, if TIMEOUT reached or error occured.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
static int telnet_recv_str(telnet_board_data *data,
                           const char *expected, char *buff)
{
    int retval;
    int offset;
    int s;
    struct timeval tv;

    s           = get_sock(&data->tcp_conn);
    tv.tv_sec   = TIMEOUT;
    tv.tv_usec  = 0;
    offset      = 0;

    retval = setsockopt(s, SOL_SOCKET, SO_RCVTIMEO, (char *)&tv, sizeof(tv));
    if (retval)
    {
        perror("telnet: setsockopt()");
        return retval;
    }

    while (1)
    {
        retval = recv(s, buff + offset, MAX_RECV_BUFF_SIZE, 0);
        if (retval == -1)
        {
            perror("telnet: recv()");
            break;
        }

        offset += retval;

        if (strstr(buff, expected))
            return 0;
    }

    return retval;
}

/**
 * Send string 'str' followed by CR to telnet server.
 *
 * @return
 *      Zero on success, or -1, if error occurred.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
static int telnet_send_str(telnet_board_data *data, const char *str)
{
    int retval;
    int s;

    s = get_sock(&data->tcp_conn);

    retval = send(s, str, strlen(str), 0);
    if (retval == -1)
    {
        perror("telnet: send()");
        return retval;
    }

    retval = send(s, "\r", 1, 0);
    if (retval == -1)
    {
        perror("telnet: send()");
        return retval;
    }

    return 0;
}

/**
 * Authorise on telnet server by username and password specified in 'data'.
 * Different telnet servers could have different prompts for users,
 * so we must also specify string we are waiting for.
 *
 * @return
 *      Zero on success, or -1, if error occurred.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
int telnet_auth(telnet_board_data *data)
{
    int  retval;
    char recv_buff[MAX_RECV_BUFF_SIZE] = {0};

    retval = socket_connect(&data->tcp_conn);
    if (retval)
        return retval;

    retval = telnet_recv_str(data, data->opt->login_prompt, recv_buff);
    if (retval)
        return retval;

    memset(recv_buff, 0, MAX_RECV_BUFF_SIZE);

    retval = telnet_send_str(data, data->opt->username);
    if (retval)
        return retval;

    retval = telnet_recv_str(data, data->opt->password_prompt, recv_buff);
    if (retval)
        return retval;

    memset(recv_buff, 0, MAX_RECV_BUFF_SIZE);

    retval = telnet_send_str(data, data->opt->password);
    if (retval)
        return retval;

    retval = telnet_recv_str(data, data->opt->cl_prompt, recv_buff);
    if (retval)
    {
        fprintf(stderr, "Error occured. Telnet output:\n"
                        "----------------------------------\n%s\n"
                        "----------------------------------\n", recv_buff);
        return retval;
    }

    return retval;
}

/**
 * Execute command specified in 'cmd_data' on telnet server.
 *
 * @return
 *      Zero on success, or -1, if error occurred.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
int telnet_execute_command(telnet_board_data *data, telnet_cmd_data *cmd_data)
{
    int  retval;
    char recv_buff[MAX_RECV_BUFF_SIZE] = {0};

    retval = telnet_send_str(data, cmd_data->command);
    if (retval)
        return retval;

    retval = telnet_recv_str(data, data->opt->cl_prompt, recv_buff);
    if (retval)
        return retval;

    if (strstr(recv_buff, cmd_data->error_substr))
    {
        fprintf(stderr, "Error occured. Telnet output:\n"
                        "----------------------------------\n%s\n"
                        "----------------------------------\n", recv_buff);
        return -1;
    }

    return retval;
}
