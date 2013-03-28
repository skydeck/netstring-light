enum authdh_namekind {
    ADN_FULLNAME = 0,
    ADN_NICKNAME = 1
};

typedef opaque des_block[8]; /* 64-bit block of encrypted data */

const MAXNETNAMELEN = 255;   /* maximum length of a netname */

union authdh_cred switch (authdh_namekind namekind) {
 case ADN_FULLNAME:
     authdh_fullname fullname;
 case ADN_NICKNAME:
     authdh_nickname nickname;
};

/* The following union is unused. */
/*
union authdh_verf switch (authdh_namekind namekind) {
 case ADN_FULLNAME:
     authdh_fullname_verf fullname_verf;
 case ADN_NICKNAME:
     authdh_nickname_verf nickname_verf;
};
*/

struct authdh_fullname {
    string name<MAXNETNAMELEN>;  /* netname of client             */
    des_block key;               /* encrypted conversation key    */
    opaque w1[4];                /* W1                            */
};

struct authdh_fullname_verf {
    des_block timestamp => full_ts;  /* T (the 64 bits of T1 and T2) */
    opaque w2[4];                /* W2                           */
};

struct authdh_nickname {
    unsigned int nickname;       /* nickname returned by server   */
};

struct authdh_nickname_verf {
    des_block timestamp => nick_ts;  /* T (the 64 bits of T1 and T2) */
    opaque w[4];                 /* Set to zero                  */
};

struct authdh_server_verf {
    des_block timestamp_verf; /* timestamp verifier (encrypted)    */
    unsigned int nickname => new_nickname;  /* new client nickname (unencrypted) */
};

struct authdh_timestamp {
    unsigned int seconds;
    int useconds;
    int ttl;
    int ttl_1;
};
