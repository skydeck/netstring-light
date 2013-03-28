/* From RFC 1833 */

const PMAP_PORT = 111;      /* portmapper port number */

struct mapping {
    unsigned int prog;
    unsigned int vers;
    unsigned int prot;
    unsigned int port;
};

const IPPROTO_TCP = 6;      /* protocol number for TCP/IP */
const IPPROTO_UDP = 17;     /* protocol number for UDP/IP */

struct pmaplist {
    mapping map;
    pmaplist *next;
};

typedef pmaplist * pmaplist_p;

struct call_args {
    unsigned int call_prog;
    unsigned int call_vers;
    unsigned int call_proc;
    opaque call_args<>;
};

struct call_result {
    unsigned int call_port;
    opaque call_res<>;
};

program PMAP {
    version V2 {
        void
        PMAPPROC_NULL(void)         = 0;

        bool
        PMAPPROC_SET(mapping)       = 1;

        bool
        PMAPPROC_UNSET(mapping)     = 2;

        unsigned int
        PMAPPROC_GETPORT(mapping)   = 3;

        pmaplist_p
        PMAPPROC_DUMP(void)         = 4;

        call_result
        PMAPPROC_CALLIT(call_args)  = 5;
   } = 2;
} = 100000;
