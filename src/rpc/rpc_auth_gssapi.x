/* $Id: rpc_auth_gssapi.x 1562 2011-03-07 16:13:14Z gerd $ -*- c -*- */


/* RPCSEC_GSS control procedures */

enum rpc_gss_proc_enum_t {
    RPCSEC_GSS_DATA = 0,
    RPCSEC_GSS_INIT = 1,
    RPCSEC_GSS_CONTINUE_INIT = 2,
    RPCSEC_GSS_DESTROY = 3
};

union rpc_gss_proc_t switch (rpc_gss_proc_enum_t d) {
case RPCSEC_GSS_DATA:
    void;
default:
    void;
};


/* RPCSEC_GSS services */

enum rpc_gss_service_enum_t {
    /* Note: the enumerated value for 0 is reserved. */
    rpc_gss_svc_none = 1,
    rpc_gss_svc_integrity = 2,
    rpc_gss_svc_privacy = 3
};

union rpc_gss_service_t switch (rpc_gss_service_enum_t d) {
case rpc_gss_svc_none:
    void;
default:
    void;
};

/* Credential */

#define RPCSEC_GSS_VERS_1 1

union rpc_gss_cred_t switch (unsigned int vers) { /* version of
					     RPCSEC_GSS */
case RPCSEC_GSS_VERS_1:
    struct {
	rpc_gss_proc_t gss_proc;  /* control procedure */
	unsigned int seq_num;   /* sequence number */
	rpc_gss_service_t service; /* service used */
	opaque handle<>;       /* context handle */
    } rpc_gss_cred_vers_1_t;
};

struct rpc_gss_init_arg {
    opaque gss_token<>;
};

struct rpc_gss_init_res {
    opaque res_handle<>;
    unsigned int res_major;
    unsigned int res_minor;
    unsigned int res_seq_window;
    opaque res_token<>;
};


struct rpc_gss_integ_data {
    _managed string databody_integ<>;
    opaque checksum<>;
};

struct rpc_gss_priv_data {
    _managed string databody_priv<>;
};


/* Major status: */
const GSS_S_COMPLETE         = 0x00000000;
const GSS_S_CONTINUE_NEEDED  = 0x00000001;

const zero = 0;

/* Maximum sequence number value */

const MAXSEQ = 0x80000000;

