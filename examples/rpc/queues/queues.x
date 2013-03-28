/* $Id: queues.x 258 2004-05-25 16:49:11Z gerd $
 * ----------------------------------------------------------------------
 *
 */

/**********************************************************************/
/* Data structures                                                    */
/**********************************************************************/

typedef string netname<255>;
/* A network-wide user name */

typedef string queuename<30>;
/* The name of a queue is a string of up to 30 bytes */

typedef opaque qid<>;
/* The queue identifier is an opaque string */

struct queueparam {
    int            qmaxlen;
    /* Maximum length of the queue. Negative means infinite.
     * Currently picked entries count.
     */
    bool           qactive;     /* Whether the queue is active */
    bool           qaccepting;  /* The queue accepts new entries */
    bool           qdelivering; /* The queue delivers entries */
    /* Difference qactive/qaccepting/qdelivering:
     * - An inactive queue indicates the error INACTIVE if somebody tries to
     *	 pick an entry or tries to upload an entry. The other operations
     *   (list entries, delete entries) are possible.
     * - Active queues can be stopped. The operations "pick" and "upload"
     *   do not fail, but they wait until the queue is started again.
     *   By setting qaccepting to false the operation "upload" is stopped,
     *   and by setting qdelivering to false the operation "pick" is stopped.
     *   You cannot stop uploads and downloads that have already begun.
     */
};


struct queue {
    qid            qid;
    queuename      qname;
    netname        qowner;
    unsigned hyper qcreation;   /* When the queue was created. Seconds since
				 * epoch.
				 */
    unsigned hyper qmodified;   /* When the queue was modified. */
    int            qlength;     /* The current length of the queue (no picked) */
    int            qpicked;     /* The number of picked entries */
    int            quploads;    /* The number of upload handles */
    queueparam     qparams;     /* The parameters (mutable) */
};
/* The struct queue contains global information about a queue. */

typedef queue qlist<>;
/* qlist is the list of all queues */

struct property {
    string pname<>;
    string pvalue<>;
};
/* A property is a pair of a name and a value that can be stored in
 * entries
 */

typedef property properties<>;
/* properties: A list of properties of arbitrary length */

typedef opaque eid<>;
/* The entry identifier is an opaque string */

struct entry {
    eid            eid;          /* The entry identifier */
    qid            eqid;         /* The identifier of the containing queue */
    unsigned hyper ecreation;    /* The creation date of the entry */
    unsigned hyper esize;        /* The size of the file in bytes */
    properties     eprops;       /* The properties */
};

typedef entry elist<>;
/* The elist is the list of entries of a queue. The first member of the
 * elist is the next entry to pick
 */

typedef opaque dhandle<>;
/* A download handle is an opaque string */

typedef opaque uhandle<>;
/* An upload handle is an opaque string */

struct chunk {
    unsigned hyper serial;  /* The serial number of the chunk */
    bool           last;    /* Is this the last chunk? */
    string         data<>;  /* The data of the chunk */
};
/* Upload and download chunks */

typedef int timeout;
/* Timeout values in seconds. Negative values mean infinite timeout, 0 means
 * immediate timeout.
 */

/**********************************************************************/
/* Error codes                                                        */
/**********************************************************************/

enum errorcode {
    SUCCESSFUL = 0,
    NOT_FOUND = 1,
    /* The queuename, qid, eid, dhandle, or uhandle could not be found */
    PICKED = 2,
    /* The entry is currently "picked" and not accessible */
    EXISTS = 3,
    /* A queue with this queuename already exists */
    NOT_PICKED = 4,
    /* The entry must be picked first in order to do this function. This code
     * is also returned if the picked entry is lost because of a timeout or
     * broken network connections.
     */
    CHUNKS_TOO_LARGE = 5,
    /* The requested chunksize is too large. */
    TIMEOUT = 6,
    /* The upload/download handle has been dropped (timeout). */
    EMPTY = 7,
    /* The queue is empty, and there is no entry to pick */
    QUEUE_DELETED = 8,
    /* While uploading/downloading someone deleted the queue */
    FULL = 9,
    /* The queue is full */
    BAD_NAME = 10,
    /* The queue name contains bad characters */
    BAD_VALUE = 11,
    /* One of the passed values is out of the valid range */
    INACTIVE = 12,
    /* The queue is inactive */
    PERMISSION_DENIED = 13,
    /* No permission for this operation */
    SYS_ERROR = 20
    /* The operating system reports an error. */
};

/**********************************************************************/
/* Result structures                                                  */
/**********************************************************************/

union errorcond switch (errorcode e) {
 case SUCCESSFUL:
     void;
 default:
     void;
};


union qlist_or_errorcode switch (errorcode e) {
 case SUCCESSFUL:
     qlist list;
 default:
     void;
};


union queue_or_errorcode switch (errorcode e) {
 case SUCCESSFUL:
     queue queue;
 default:
     void;
};


union elist_or_errorcode switch (errorcode e) {
 case SUCCESSFUL:
     elist list;
 default:
     void;
};


union entry_or_errorcode switch (errorcode e) {
 case SUCCESSFUL:
     entry entry;
 default:
     void;
};


union dhandle_or_errorcode switch (errorcode e) {
 case SUCCESSFUL:
     dhandle dhandle;
 default:
     void;
};


union uhandle_or_errorcode switch (errorcode e) {
 case SUCCESSFUL:
     uhandle uhandle;
 default:
     void;
};


union chunk_or_errorcode switch (errorcode e) {
 case SUCCESSFUL:
     chunk chunk;
 default:
     void;
};

/**********************************************************************/
/* Program                                                            */
/**********************************************************************/

program QUEUESPROG {
    version QUEUESVERS1 {

	/* Ping */
	void ping(void) = 0;

	/* Management of queues */
	errorcond               create_queue(queuename) = 10;
	errorcond               delete_queue(queuename) = 11;
	qlist_or_errorcode      list_queues(void) = 12;
	queue_or_errorcode      get_queue(queuename) = 13;
        errorcond               set_queue(queuename, queueparam) = 14;

	/* Queue entries */
	elist_or_errorcode      list_queue_entries(qid) = 20;
	entry_or_errorcode      pick_queue_entry(qid,timeout) = 21;
	errorcond               return_picked_queue_entry(qid,eid) = 22;
	errorcond               remove_picked_queue_entry(qid,eid) = 23;
	errorcond               remove_queue_entry(qid,eid) = 24;

	/* A "picked" entry is temporarily removed from the queue. It can
	 * be downloaded, but is invisible to others. By removing the
	 * picked entry it is really removed from the queue. By returning
	 * the entry it is visible again.
	 * If the queue is empty, the timeout value specifies how long to
	 * wait for new entries. 0 means not to wait. A negative value means
	 * to wait endless. If there is still no entry after this time
	 * the error EMPTY is returned.
	 * If the TCP connection is closed, the picked entry will be
	 * automatically returned to the queue.
	 */

	/* Download queue entries */
	dhandle_or_errorcode    download_entry(qid,eid,int) = 30;
	chunk_or_errorcode      download_chunk(dhandle) = 31;

	/* You can only download picked entries. If the entry is
	 * removed or returned while the download is in progress, the
	 * download functions will indicate an error condition.
	 *
	 * download_entry(qid,eid,chunksize): The function requests a download
	 *   in chunks of [chunksize] bytes.
	 * download_chunk(dhandle): Downloads the next chunk. The dhandle
	 *   is automatically deallocated when the last chunk is downloaded,
	 *   or when the picked entry is removed or returned.
	 */

	/* Upload queue entries */
	uhandle_or_errorcode    upload_entry(qid,properties, timeout) = 40;
	errorcond               upload_chunk(uhandle, chunk) = 41;

	/* Only if the entry is completely uploaded, it will be visible
	 * to the others.
	 * If the upload is not finished until timeout, the upload will
	 * be canceled. The same happens if the TCP connection breaks.
	 *
	 * upload_entry(qid,properties, timeout): Begins an upload. The entry
	 *   will have the passed property list.
	 * upload_chunk(uhandle, chunk): Uploads the next chunk. The chunks
	 *   must have proper serial numbers, and the last chunk must be
	 *   flagged as last.
	 */
    } = 1;
} = 0x2000EEEE;
