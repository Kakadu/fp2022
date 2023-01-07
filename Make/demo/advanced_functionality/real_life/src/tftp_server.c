#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdint.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>

#include "include/tftp_server.h"

#define RECV_TIMEOUT            5
#define RECV_RETRIES            5
#define TFTP_MAX_PAYLOAD        512
#define TFTP_MSG_MIN_SIZE       4

enum tftp_opcode {
    RRQ = 1,
    WRQ,
    DATA,
    ACK,
    ERROR
};

enum tftp_transfer_mode {
    NETASCII = 1,
    OCTET
};

typedef union {

    uint16_t opcode;

    struct {
        uint16_t    opcode;                                 /* RRQ or WRQ */
        uint8_t     filename_and_mode[TFTP_MAX_PAYLOAD + 2];/* +2 for mode */
    } __attribute__((packed)) request;

    struct {
        uint16_t    opcode;                                 /* DATA */
        uint16_t    block_number;
        uint8_t     data[TFTP_MAX_PAYLOAD];
    } __attribute__((packed)) data;

    struct {
        uint16_t    opcode;                                 /* ACK */
        uint16_t    block_number;
    } __attribute__((packed)) ack;

    struct {
        uint16_t    opcode;                                 /* ERROR */
        uint16_t    error_code;
        uint8_t     error_string[TFTP_MAX_PAYLOAD];
    } __attribute__((packed)) error;

} __attribute__((packed)) tftp_message;

/* Global variable that helps properly close the server. */
int global_server_socket;

/** Close tftp server.  */
void term_handler()
{
    printf("tftp server: shutting down\n");

    close(global_server_socket);

    exit(EXIT_SUCCESS);
}

/** Handle error in child proccess */
void chld_handler()
{
    int status;
    wait(&status);

    /* handle error here */
}

/**
 * Fill 'ret' structure with appropriate data.
 *
 * @return
 *      Zero on success, or -1, if error occured.
 */
int tftp_fill_server_data(tftp_server_data *ret, tftp_server_options *opt)
{
    int retval;

    retval = conn_info_fill(&ret->udp_conn, opt->addr,
                            atoi(opt->port), SOCK_DGRAM);
    if (retval)
        return retval;

    ret->base_directory = opt->dir;

    return retval;
}

/**
 * Send tftp DATA packet to client.
 *
 * @return
 *      Zero on success, or -1, if error occured.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
static int tftp_send_data(int s, uint16_t block_number, uint8_t *data,
                          ssize_t data_len, struct sockaddr_in *sock,
                          socklen_t slen)
{
    tftp_message    msg;
    int             msg_len;
    ssize_t         len;

    msg.opcode              = htons(DATA);
    msg.data.block_number   = htons(block_number);
    msg_len                 = 4 + data_len;                /* +4 for opcode */
    memcpy(msg.data.data, data, data_len);

    len = sendto(s, &msg, msg_len, 0, (struct sockaddr *)sock, slen);
    if (len < 0)
    {
        perror("tftp server: sendto()");
        return -1;
    }

    return 0;
}

/**
 * Send tftp ACK packet to client.
 *
 * @return
 *      Zero on success, or -1, if error occured.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
static int tftp_send_ack(int s, uint16_t block_number,
                         struct sockaddr_in *sock, socklen_t slen)
{
    tftp_message    msg;
    int             msg_len;
    ssize_t         len;

    msg.opcode              = htons(ACK);
    msg.ack.block_number    = htons(block_number);
    msg_len                 = sizeof(msg.ack);

    len = sendto(s, &msg, msg_len, 0, (struct sockaddr *)sock, slen);
    if (len < 0)
    {
        perror("tftp server: sendto()");
        return -1;
    }

    return 0;
}

/**
 * Send tftp ERROR packet to client.
 *
 * @return
 *      Zero on success, or -1, if error occured.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
static int tftp_send_error(int s, int error_code, char *error_string,
                           struct sockaddr_in *sock, socklen_t slen)
{
    tftp_message    msg;
    int             msg_len;
    ssize_t         len;

    if(strlen(error_string) >= TFTP_MAX_PAYLOAD)
    {
        fprintf(stderr, "tftp server: error string too long\n");
        return -1;
    }

    msg.opcode              = htons(ERROR);
    msg.error.error_code    = error_code;
    msg_len                 = 4 + strlen(error_string) + 1;
    /* +4 for opcode, +1 for error_code */

    strcpy((char *)msg.error.error_string, error_string);

    len = sendto(s, &msg, msg_len, 0, (struct sockaddr *)sock, slen);
    if (len < 0)
    {
        perror("tftp server: sendto()");
        return -1;
    }

    return 0;
}

/**
 * Receive message from client and put it into 'msg'.
 *
 * @return
 *      Length of the message on successful completion.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
static ssize_t tftp_recv_message(int s, tftp_message *msg,
                                 struct sockaddr_in *sock, socklen_t *slen)
{
    ssize_t len;

    len = recvfrom(s, msg, sizeof(*msg), 0, (struct sockaddr *)sock, slen);
    if (len < 0 && errno != EAGAIN)
    {
        perror("tftp server: recvfrom()");
    }

    return len;
}

/**
 * Create socket to handle client request.
 *
 * @return
 *      Zero on success, or -1, if error occured.
 *
 * @se
 *      Prints information about occurred error to stderr.
 *      If an error occure, causes process termination.
 */
static int tftp_socket_create(void)
{
    int s;
    struct timeval tv;

    s = socket(AF_INET, SOCK_DGRAM, 0);
    if (s == -1)
    {
        perror("tftp server: socket()");
        exit(EXIT_FAILURE);
    }

    tv.tv_sec  = RECV_TIMEOUT;
    tv.tv_usec = 0;

    if(setsockopt(s, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)))
    {
        perror("tftp server: setsockopt()");
        exit(EXIT_FAILURE);
    }

    return s;
}

/**
 * Check if the 'filename' is valid.
 *
 * @param filename              Filename from client request.
 * @param base_directory        Server specified directory.
 *
 * @return
 *      Zero on success, or nonzero, if the filename is invalid.
 *
 * @alg
 *      Result consists of three checks:
 *      First check  - if the filename starts with "../",
 *      then it's definetly outside base directory.
 *      Second check - "/../" should not be used.
 *      Third check  - if the filename starts with "/", then
 *      the only acceptable way is the way when filename starts
 *      with fill path to the base directory.
 */
static int filename_check(char *filename, const char *base_directory)
{
    int first_check;
    int second_check;
    int third_check;

    first_check  = (strncmp(filename, "../", 3) == 0);
    second_check = (strstr(filename, "/../") != NULL);
    third_check  = (strncmp(filename, base_directory, strlen(base_directory)) &&
                   filename[0] == '/');

    return first_check || second_check || third_check;
}

/**
 * Get opcode, filename and mode from tftp request.
 *
 * @param base_directory        Server specified directory.
 * @param msg                   Request from client.
 *
 * @return
 *      Pointer to string with information about
 *      occured error, or NULL otherwise.
 */
static char* tftp_get_request_data(uint16_t *opcode, char **filename,
                                   int *mode, const char *base_directory,
                                   tftp_message *msg, ssize_t msg_len)
{
    char *request_last_byte;
    char *mode_string;

    /*
     *             TFTP request packet structure:
     *   2 bytes     string    1 byte     string   1 byte
     *   ------------------------------------------------
     *  | Opcode |  Filename  |   0  |    Mode    |   0  |
     *   ------------------------------------------------
     */

    *filename           = (char *)msg->request.filename_and_mode;
    request_last_byte   = *filename + msg_len - 2 - 1;

    if (*request_last_byte != '\0')
        return "invalid filename or mode";

    mode_string = strchr(*filename, '\0') + 1;

    if (mode_string > request_last_byte)
        return "transfer mode not specified";

    if (filename_check(*filename, base_directory) != 0)
        return "filename outside base directory";

    *opcode = ntohs(msg->opcode);
    *mode   = !strcasecmp(mode_string, "netascii") ? NETASCII :
             (!strcasecmp(mode_string, "octet")    ? OCTET    : 0);

    if (*mode == 0)
        return "invalid transfer mode";

    return NULL;
}

/**
 * Send file from file descriptor 'fd' to client.
 *
 * @se
 *      If an server error occure, function will
 *      print information about it to stderr and cause process termination.
 *
 * @return
 *      Pointer to string with information about
 *      occured error, or NULL otherwise.
 */
static char* tftp_handle_read_request(int s, FILE *fd,
                                      struct sockaddr_in *client_sock,
                                      socklen_t slen)
{
    tftp_message    msg;
    ssize_t         msg_len;
    uint8_t         data[TFTP_MAX_PAYLOAD];
    ssize_t         data_len;
    uint16_t        block_number = 0;
    int             rc;
    int             countdown;

    do {
        data_len = fread(data, 1, sizeof(data), fd);
        block_number++;

        for (countdown = RECV_RETRIES; countdown; countdown--)
        {
            rc = tftp_send_data(s, block_number, data, data_len,
                                client_sock, slen);
            if (rc)
            {
                printf("%s.%u: transfer killed\n",
                        inet_ntoa(client_sock->sin_addr),
                        ntohs(client_sock->sin_port));
                exit(EXIT_FAILURE);
            }

            msg_len = tftp_recv_message(s, &msg, client_sock, &slen);

            if (msg_len < 0 && errno != EAGAIN)
            {
                printf("%s.%u: transfer killed\n",
                        inet_ntoa(client_sock->sin_addr),
                        ntohs(client_sock->sin_port));
                exit(EXIT_FAILURE);
            }
            else if (msg_len >= 0 && msg_len < TFTP_MSG_MIN_SIZE)
                return "message with invalid size received";
            else
                break;
        }

        if (!countdown)
        {
            printf("%s.%u: transfer timed out\n",
                    inet_ntoa(client_sock->sin_addr),
                    ntohs(client_sock->sin_port));
            exit(EXIT_FAILURE);
        }

        if (ntohs(msg.opcode) == ERROR)
        {
            printf("%s.%u: error message received: %u %s\n",
                    inet_ntoa(client_sock->sin_addr),
                    ntohs(client_sock->sin_port),
                    ntohs(msg.error.error_code), msg.error.error_string);
            exit(EXIT_FAILURE);
        }

        if (ntohs(msg.opcode) != ACK)
            return "invalid message during transfer received";

        /* the ack number is too high */
        if (ntohs(msg.ack.block_number) != block_number)
            return "invalid ack number received";

    } while (data_len >= TFTP_MAX_PAYLOAD);

    return NULL;
}

/**
 * Receive data from client and write it to file descriptor 'fd'.
 *
 * @se
 *      If an server error occure, function will
 *      print information about it to stderr and cause process termination.
 *
 * @return
 *      Pointer to string with information about
 *      occured error, or NULL otherwise.
 */
static char* tftp_handle_write_request(int s, FILE *fd,
                                       struct sockaddr_in *client_sock,
                                       socklen_t slen)
{
    tftp_message        msg;
    ssize_t             msg_len;
    uint16_t            block_number = 0;
    int                 rc;
    int                 countdown;

    rc = tftp_send_ack(s, block_number, client_sock, slen);
    if (rc)
    {
        printf("%s.%u: transfer killed\n",
               inet_ntoa(client_sock->sin_addr),
               ntohs(client_sock->sin_port));
        exit(EXIT_FAILURE);
    }

    do {
        for (countdown = RECV_RETRIES; countdown; countdown--)
        {
            msg_len = tftp_recv_message(s, &msg, client_sock, &slen);

            if (msg_len < 0 && errno != EAGAIN)
            {
                printf("%s.%u: transfer killed\n",
                        inet_ntoa(client_sock->sin_addr),
                        ntohs(client_sock->sin_port));
                exit(EXIT_FAILURE);
            }
            else if (msg_len < 0)
            {
                rc = tftp_send_ack(s, block_number, client_sock, slen);
                if (rc)
                {
                    printf("%s.%u: transfer killed\n",
                            inet_ntoa(client_sock->sin_addr),
                            ntohs(client_sock->sin_port));
                    exit(EXIT_FAILURE);
                }
            }
            else if (msg_len >= 0 && msg_len < TFTP_MSG_MIN_SIZE)
                return "message with invalid size received";
            else
                break;
        }

        if (!countdown)
        {
            printf("%s.%u: transfer timed out\n",
                    inet_ntoa(client_sock->sin_addr),
                    ntohs(client_sock->sin_port));
            exit(EXIT_FAILURE);
        }

        block_number++;

        if (ntohs(msg.opcode) == ERROR)  {
            printf("%s.%u: error message received: %u %s\n",
                    inet_ntoa(client_sock->sin_addr),
                    ntohs(client_sock->sin_port),
                    ntohs(msg.error.error_code), msg.error.error_string);
            exit(EXIT_FAILURE);
        }

        if (ntohs(msg.opcode) != DATA)
            return "invalid message during transfer received";

        if (ntohs(msg.ack.block_number) != block_number)
            return "invalid block number received";

        rc = fwrite(msg.data.data, 1, msg_len - 4, fd);     /* +4 for opcode */
        if (rc < 0)
        {
            perror("tftp server: fwrite()");
            exit(EXIT_FAILURE);
        }

        rc = tftp_send_ack(s, block_number, client_sock, slen);
        if (rc)
        {
            printf("%s.%u: transfer killed\n",
                    inet_ntoa(client_sock->sin_addr),
                    ntohs(client_sock->sin_port));
            exit(EXIT_FAILURE);
        }

    } while ((size_t)msg_len >= sizeof(msg.data));
    /* since msg_len is non-negative, we can safely cast it to size_t */

    return NULL;
}

/**
 * Opens new socket on new port to handle tftp client request.
 * The function will run in a child process created by tftp_server_start().
 *
 * @param base_directory        Server specified directory.
 *
 * @se
 *      Prints information about start and end of the transfer.
 *      If an error occure, function will:
 *      Print information about it to stderr,
 *      send string to client containing error description,
 *      cause process termination.
 */
void tftp_handle_request(tftp_message *msg, ssize_t msg_len,
                         const char *base_directory,
                         struct sockaddr_in *client_sock,
                         socklen_t slen)
{
    int         s;
    int         mode;
    char        *filename;
    char        *error_string;
    uint16_t    opcode;
    FILE        *fd;

    s = tftp_socket_create();

    error_string = tftp_get_request_data(&opcode, &filename, &mode,
                                         base_directory, msg, msg_len);
    if (error_string != NULL)
    {
        printf("%s.%u: %s\n",
                inet_ntoa(client_sock->sin_addr),
                ntohs(client_sock->sin_port),
                error_string);
        tftp_send_error(s, 0, error_string, client_sock, slen);
        close(s);
        exit(EXIT_FAILURE);
    }

    fd = fopen(filename, opcode == RRQ ? "r" : "w");
    if (fd == NULL)
    {
        perror("tftp server: fopen()");
        tftp_send_error(s, errno, strerror(errno), client_sock, slen);
        close(s);
        exit(EXIT_FAILURE);
    }

    printf("%s.%u: request received: %s '%s' %s\n",
            inet_ntoa(client_sock->sin_addr), ntohs(client_sock->sin_port),
            opcode == RRQ   ? "get"   : "put", filename,
            mode   == OCTET ? "oktet" : "netascii");

    if (opcode == RRQ)
        error_string = tftp_handle_read_request(s, fd, client_sock, slen);
    else if (opcode == WRQ)
        error_string = tftp_handle_write_request(s, fd, client_sock, slen);

    if (error_string != NULL)
    {
        printf("%s.%u: %s\n",
               inet_ntoa(client_sock->sin_addr),
               ntohs(client_sock->sin_port),
               error_string);
        tftp_send_error(s, 0, error_string, client_sock, slen);
        fclose(fd);
        close(s);
        exit(EXIT_FAILURE);
    }

    printf("%s.%u: '%s' transfer completed\n",
            inet_ntoa(client_sock->sin_addr),
            ntohs(client_sock->sin_port),
            filename);

    fclose(fd);
    close(s);
}

/**
 * Start tftp server on ip, port and directory specified in
 * 'srv_data' structure.
 * Server stops by sending signal SIGTERM to it.
 *
 * @se
 *      Prints information about occurred error to stderr.
 *      If an error occures in received message, prints
 *      information about that in stdout and sends back ERROR packet.
 */
int tftp_server_start(tftp_server_data *srv_data)
{
    int                 s;
    int                 retval;
    ssize_t             msg_len;
    struct sockaddr_in  client_sock;
    socklen_t           slen;

    s      = get_sock(&srv_data->udp_conn);
    slen   = sizeof(client_sock);

    retval = chdir(srv_data->base_directory);
    if (retval)
    {
        perror("tftp server: chdir()");
        return retval;
    }

    retval = socket_bind(&srv_data->udp_conn);
    if (retval)
        return retval;

    global_server_socket = s;

    signal(SIGCHLD, (void *) chld_handler);
    signal(SIGTERM, (void *) term_handler);

    printf("tftp server: listening on %d\n", get_port(&srv_data->udp_conn));

    while (1)
    {
        tftp_message msg;

        msg_len = tftp_recv_message(s, &msg, &client_sock, &slen);

        if (msg_len < 0)
            continue;

        if (msg_len < TFTP_MSG_MIN_SIZE)
        {
            printf("%s.%u: request with invalid size received\n",
                    inet_ntoa(client_sock.sin_addr),
                    ntohs(client_sock.sin_port));
            tftp_send_error(s, 0, "invalid request size", &client_sock, slen);
            continue;
        }

        if (ntohs(msg.opcode) == RRQ || ntohs(msg.opcode) == WRQ)
        {
            if (fork() == 0)
            {
                tftp_handle_request(&msg, msg_len, srv_data->base_directory,
                                    &client_sock, slen);
                exit(EXIT_SUCCESS);
            }
        }
        else
        {
            printf("%s.%u: invalid request received: %d\n",
                    inet_ntoa(client_sock.sin_addr),
                    ntohs(client_sock.sin_port), ntohs(msg.opcode));
            tftp_send_error(s, 0, "invalid opcode", &client_sock, slen);
        }
    }
}
