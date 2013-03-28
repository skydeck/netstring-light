/* -*- c -*- */

typedef string longstring<>;     /* just a string */
typedef longstring sortdata<>;   /* an array of strings to sort, or sorted */
typedef longstring endpoint;     /* the name of a socket */
typedef longstring endpoints<>;  /* several sockets */

struct shm {
    longstring shm_name;         /* the global name of the shm object */
    hyper      shm_addr;         /* the mapping address */
};

/* The Worker program is implemented by the N worker processes. */

program Worker {
    version V1 {
        void null(void) = 0;

	shm alloc_shm(hyper) = 1;
	/* Requests that the worker allocates a shared memory block of the
           passed size
	*/

        void sort_shm(shm, int) = 2;
	/* sort_shm(shm,offset):
           Requests to sort the data in this shared memory block. The data
           is represented in the internal memory format that O'Caml uses
           for "string array". The int is the byte offset where the value
	   starts in the memory block.
	*/

	int copy_shm(shm, int, shm) = 3;
	/* copy_shm(shm1,offset1,shm2):
           Copies the data in the first memory block to the second memory
           block which must have the same size. The first shm block must
           have been allocated with alloc_shm. The second block can be any
           block, and it can be mapped at any address. The data in the block
           must be a "string array" starting at offset1.

           The copy is put at the beginning of shm2. The returned int is
           the byte offset where the value begins.
	*/

	void free_shm(shm) = 4;
	/* Frees this shm block */
    } = 1;
} = 1;


/* The external interface - implemented by the single controller process */

program Interface {
    version V1 {
        void null(void) = 0;
        
        sortdata sort(sortdata, bool) = 1;
        /* Sort this array and return it. The bool says whether to sort (true) -
           if false, the input data are just returned
         */

	endpoints get_workers(void) = 2;
	/* Get the worker sockets - for bypassing the controller */
    } = 1;
} = 3;
