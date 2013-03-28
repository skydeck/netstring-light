/* $Id: finder_service.x 1199 2008-07-27 15:43:11Z gerd $ -*- c -*- */

typedef string longstring<>;
/* A longstring is a string with unlimited length. Actually there is a 4GB
 * limit in the RPC protocol.
 */

enum location_enum {
    NOT_FOUND = 0,
    FOUND = 1
};

union location switch (location_enum discr) {
 case NOT_FOUND:
     void;
 case FOUND:
     longstring pathname;
};

program Finder {
    version V1 {
	void ping(void) = 0;

	location find(longstring) = 1;
	/* Searches for the file with this name (like "find -name filename").
         * The returned location is:
         * - NOT_FOUND if the file could not be not found
         * - FOUND if found, and the attached pathname shows where. Only
         *   the first match is returned.
         */

	longstring lastquery(void) = 2;
	/* Simply returns the last query (example for Netplex_sharedvar) */

	void shutdown(void) = 3;
	/* Shut the Netplex system down. Note that you can do this also by
           calling the netplex-admin utility
	*/
    } = 1;
} = 200000;
