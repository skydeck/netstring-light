/* -*- c -*- */

typedef double row<>;

struct dim {
    int rows;
    int columns;
};


struct job {
    int left_col;
    int right_row;
    /* Multiply which column of the left matrix with which row of the right
       matrix
    */
};

typedef job jobs<>;

struct result {
    job res_job;
    double res_val;
};

typedef result results<>;

enum which { left=0, right=1 };


program Multiplier {
    version V1 {
	void ping(void) = 0;
	
	void test_multiply(int,int,int) = 1;
	/* Creates a test matrix with random values and multiplies them.
	   Args are: (l_rows, r_cols, l_cols = r_rows)
	*/
    } = 1;
} = 1;


program Controller {
    version V1 {
	void ping(void) = 0;

	dim get_dim(which) = 1;
	/* Workers can call this proc to get the dimension of the matrix */

	row get_row(which,int) = 2;
	/* Workers can call this proc to get a row of the matrix. The int
           is the row number, 0..rows-1
	*/

	jobs pull_jobs(int) = 3;
	/* The controller maintains a queue of jobs. This proc pulls a list
           of jobs from this queue. The int is the requested number.
	*/

	void put_results(results) = 4;
	/* Put results into the result matrix. */
	
    } = 1;
} = 2;


program Worker {
    version V1 {
	void ping(void) = 0;

	void run(void) = 1;
	/* The controller calls this proc to initiate the action in the worker.
           When it returns, it is assumed that the worker is completely
           finished with everything.
	*/
    } = 1;
} = 3;
