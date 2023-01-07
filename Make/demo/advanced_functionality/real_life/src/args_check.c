#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <regex.h>

#include "include/args_check.h"

/* Default options. */
#define STD_FLAGS                0
#define STD_BOARD_ADDR           "192.168.1.1"
#define STD_TELNET_PORT          "23"
#define STD_USERNAME             "admin"
#define STD_PASSWORD             "admin"
#define STD_LOGIN_PROMPT         "login:"
#define STD_PASSWORD_PROMPT      "Password:"
#define STD_CL_PROMPT            "root@ugwcpe:"
#define STD_HOST_ADDR            "192.168.1.3"
#define STD_TFTP_PORT            "12345"
#define STD_TFTP_DIRECTORY       "."

/* options which don't have a one-char version */
#define OPT_TFTP_DIR             256
#define OPT_CL_PROMPT            257
#define OPT_LOGIN_PROMPT         258
#define OPT_PWD_PROMPT           259

#define STD_A_ARG_VALUE          "\""STD_BOARD_ADDR":"STD_TELNET_PORT"\""
#define STD_T_ARG_VALUE          "\""STD_HOST_ADDR":"STD_TFTP_PORT"\""

#define OPTSTRING                ":hqp:u:t:a:P:"
#define BOARD_TYPES              "IAD731 IAD811"

dump_params_options global_opt;

static struct option long_opts[] =
{
    {"help",         no_argument,       0, 'h'},
    {"quiet",        no_argument,       0, 'q'},
    {"password",     required_argument, 0, 'p'},
    {"username",     required_argument, 0, 'u'},
    {"addr",         required_argument, 0, 'a'},
    {"tftp-addr",    required_argument, 0, 't'},
    {"tftp-dir",     required_argument, 0,  OPT_TFTP_DIR},
    {"cl-prompt",    required_argument, 0,  OPT_CL_PROMPT},
    {"login-prompt", required_argument, 0,  OPT_LOGIN_PROMPT},
    {"pwd-prompt",   required_argument, 0,  OPT_PWD_PROMPT},
    {0, 0, 0, 0}
};

static struct {
  int          opt;
  char * const flagdesc;
  char * const desc;
  char * const arg;
} usage[] = {
  { 'h', NULL,                 "Display this message.",                                          NULL },
  { 'q', NULL,                 "Produce no output.",                                             NULL },
  { 'u', "<username>",         "Specify username for telnet server. Default value is %s.",   "\""STD_USERNAME"\"" },
  { 'p', "<password>",         "Specify password for telnet server. Default value is %s.",   "\""STD_PASSWORD"\"" },
  { 'a', "<[ipaddr][:port]>",  "Specify board address and telnet port. Default value is %s.",    STD_A_ARG_VALUE },
  { 't', "<[ipaddr][:port]>",  "Specify address and port for tftp server. Default value is %s.", STD_T_ARG_VALUE },
  { OPT_TFTP_DIR,     "<dir>", "Specify directory for tftp server. Default value is %s.",    "\""STD_TFTP_DIRECTORY"\"" },
  { OPT_CL_PROMPT,    "<str>", "Specify command line prompt. Default value is %s.",          "\""STD_CL_PROMPT"\"" },
  { OPT_LOGIN_PROMPT, "<str>", "Specify login prompt. Default value is %s.",                 "\""STD_LOGIN_PROMPT"\"" },
  { OPT_PWD_PROMPT,   "<str>", "Specify password prompt. Default value is %s.",              "\""STD_PASSWORD_PROMPT"\"" },
  { 0, NULL, NULL, NULL }
};

static void usage_print(void)
{
    printf("Usage: ./dump_wifi_params [options] <board-type>\n");
    printf("Valid board types are: %s\n", BOARD_TYPES);
    printf("To see all possible options use ./dump_wifi_params -h\n");
}

static void opts_print(void)
{
    char buff[100];
    int i, j;

    for (i = 0; usage[i].opt != 0; i++)
    {
        char *desc = usage[i].flagdesc;
        char *eq   = "=";

        if (!desc || *desc == '[')
            eq = "";

        if (!desc)
            desc = "";

        for (j = 0; long_opts[j].name; j++)
            if (long_opts[j].val == usage[i].opt)
                break;

        if (usage[i].opt < 256)
            sprintf(buff, "-%c, ", usage[i].opt);
        else
            sprintf(buff, "    ");

        sprintf(buff + 4, "--%s%s%s", long_opts[j].name, eq, desc);
        printf("%-55.55s", buff);

        if (usage[i].arg)
            strcpy(buff, usage[i].arg);

        printf(usage[i].desc, buff);
        printf("\n");
    }
}

static void set_defaults(void)
{
    global_opt.flags                           = STD_FLAGS;
    global_opt.telnet_opt.addr                 = STD_BOARD_ADDR;
    global_opt.telnet_opt.port                 = STD_TELNET_PORT;
    global_opt.telnet_opt.username             = STD_USERNAME;
    global_opt.telnet_opt.password             = STD_PASSWORD;
    global_opt.telnet_opt.login_prompt         = STD_LOGIN_PROMPT;
    global_opt.telnet_opt.password_prompt      = STD_PASSWORD_PROMPT;
    global_opt.telnet_opt.cl_prompt            = STD_CL_PROMPT;
    global_opt.tftp_opt.addr                   = STD_HOST_ADDR;
    global_opt.tftp_opt.port                   = STD_TFTP_PORT;
    global_opt.tftp_opt.dir                    = STD_TFTP_DIRECTORY;
}

/*
 * Find next symbol 'c', split string with zero and eliminate spaces.
 *
 * @return
 *     Start of string following 'c'.
 */
static char *split_chr(char *s, char c)
{
    char *symb, *p;

    if (!s || !(symb = strchr(s, c)))
        return NULL;

    p = symb;
    *symb = ' ';

    for (; *symb == ' '; symb++);

    for (; (p >= s) && *p == ' '; p--)
        *p = 0;

    return symb;
}

/*
 * Find word 'type' in string BOARD_TYPES.
 *
 * @return
 *      Zero on success, or -1, if error occurred.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
static int type_find(const char *type)
{
    int     retval;
    char    regex[100];
    regex_t preg;

    if (strlen(type) > 50) /* 50 is just big enough number */
    {
        fprintf(stderr, "%s is not valid board type.\n", type);
        return -1;
    }

    sprintf(regex, ".*\\b%s\\b.*", type);

    retval = regcomp(&preg, regex, 0);
    if (retval)
    {
        perror("regcomp");
        regfree(&preg);
        return retval;
    }

    retval = regexec(&preg, BOARD_TYPES, 0, NULL, 0);
    if (retval == REG_NOMATCH)
        fprintf(stderr, "%s is not valid board type.\n", type);
    else if (retval)
        perror("regexec");

    regfree(&preg);
    return retval ? -1 : 0;
}

/* Fill global_opt.tftp_opt with options from optarg.  */
static void opts_parse_tftp(void)
{
    char *addr;
    char *port;

    port = split_chr(optarg, ':');
    addr = optarg;

    if (addr && *addr != '\0')
        global_opt.tftp_opt.addr = addr;

    if (port && *port != '\0')
        global_opt.tftp_opt.port = port;
}

/* Fill global_opt.telnet_opt with options from optarg.  */
static void opts_parse_telnet(void)
{
    char *addr;
    char *port;

    port = split_chr(optarg, ':');
    addr = optarg;

    if (addr && *addr != '\0')
        global_opt.telnet_opt.addr = addr;

    if (port && *port != '\0')
        global_opt.telnet_opt.port = port;
}

/**
 * Fill global_opt structure with options from command line.
 *
 * @return
 *      Zero on success, or -1, if error occurred.
 *
 * @se
 *      Prints information about occurred error to stderr.
 */
int options_get(int argc, char **argv)
{
    int opt;
    int opt_index;

    set_defaults();

    while(1)
    {
        opt = getopt_long(argc, argv, OPTSTRING, long_opts, &opt_index);

        if(opt == -1)
            break;

        switch(opt)
        {
            case 'h':
                opts_print();
                return -1;
            case 'a':
                opts_parse_telnet();
                break;
            case 't':
                opts_parse_tftp();
                break;
            case 'q':
                global_opt.flags |= FLAG_QUIET;
                break;
            case 'u':
                global_opt.telnet_opt.username = optarg;
                break;
            case 'p':
                global_opt.telnet_opt.password = optarg;
                break;
            case OPT_TFTP_DIR:
                global_opt.tftp_opt.dir = optarg;
                break;
            case OPT_CL_PROMPT:
                global_opt.telnet_opt.cl_prompt = optarg;
                break;
            case OPT_LOGIN_PROMPT:
                global_opt.telnet_opt.login_prompt = optarg;
                break;
            case OPT_PWD_PROMPT:
                global_opt.telnet_opt.password_prompt = optarg;
                break;
            case ':':
                fprintf(stderr, "Option -%c requires an argument.\n", optopt);
                goto abort;
            case '?':
                fprintf(stderr, "Unknown option: %c\n", optopt);
                goto abort;
        }
    }

    if(optind + 1 == argc)
    {
        if(type_find(argv[optind]))
            goto abort;

        global_opt.board_type = argv[optind];
    }
    else if (optind == argc)
    {
        fprintf(stderr, "Type must be specified.\n");
        goto abort;
    }
    else
    {
        fprintf(stderr, "Too much arguments.\n");
        goto abort;
    }

    return 0;

abort:
    usage_print();
    return -1;
}
