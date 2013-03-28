/* -*- c -*- */

typedef string longstring<>;     /* just a string */
typedef longstring sortdata<>;   /* an array of strings to sort, or sorted */
typedef longstring endpoint;     /* the name of a socket */


enum op_type {
    KEEP = 0,
    FORWARD = 1,
    RETURN = 2
};

union op switch (op_type d) {
 case KEEP:      /* Keep the sorted data in the worker process */
     void;
 case FORWARD:   /* Pass the sorted data to another worker, and merge there */
     struct {
	 endpoint destination;              /* the address of the worker */
	 int      merge_with_partition_id;  /* the partition to merge with */
	 int      new_partition_id;         /* name of the merged data */
	 op       continuation;             /* what to do next... */
     } s;
 case RETURN:    /* Return to caller */
     endpoint e; /* the address of the controller */
};


/* The Worker program is implemented by the N worker processes. */

program Worker {
    version V1 {
	void null(void) = 0;

	void sort_partition(int      /* partition_id */,
			    sortdata /* data */,
			    op       /* continuation */) = 1;
	/* Sort [data], which has the given [partition_id], then execute the
           operation (or series of operations) encoded in [continuation].
	*/

	void merge_partition(int      /* partition1_id */,
                             int      /* partition2_id */,
			     sortdata /* data2 */,
			     int      /* result_partition_id */,
			     op       /* continuation */) = 2;
	/* Merge the two partitions identified by [partition1_id] and
	   [partition2_id]. The first of these must either already exist
           in the worker process, or must be made available there 
           in the future (by a [sort_partition] or [merge_partition] with
           a continuation of [KEEP]). The second partition is [data2].
           The new, merged partition is assigned the new identifier
           [result_partition_id], and [continuation] says what to do next.
	*/

    } = 1;
} = 1;


/* The Controller program is implemented by the single controller process. */

program Controller {
    version V1 {
	void null(void) = 0;

	void return_result(sortdata) = 1;
	/* Return the result to the caller */
    } = 1;
} = 2;


/* The external interface - also implemented by the controller process */

program Interface {
    version V1 {
	void null(void) = 0;
	
	sortdata sort(sortdata) = 1;
	/* Sort this array and return it */
    } = 1;
} = 3;
