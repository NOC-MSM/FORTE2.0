#include <stdio.h>

#include "defns.h"
#include "structs.h"

int tmpfile_num;
FILE *logfile;
char tmpdir[12];   /* Added 14-11-95  Marc Chippindale */


main( argc, argv )
int   argc;
char  **argv;
{
	FILE *fp;
	char *pl_line[max_pl_lines];
	int  line_count = -1;
	int  i;
	char buffer[maxlen2];


	if ( argc != 2 )  {
		prt_error( "Usage: rsort file_name", "\0", user_error );
	}

	if ( access( argv[1], 4 ) == -1 )  {
		prt_error( "\nInput file is non-existent or read disallowed", argv[1],
			user_error );
	}

	nupopen( fp, argv[1], "r", system_error );


	while ( fgets( buffer, maxlen - 1, fp ) != NULL )  {

		line_count++;

		Malloc_and_copy( pl_line[ line_count ], buffer );

#ifdef DEBUG
		fprintf( stderr, "%d, %s", line_count, pl_line[ line_count ] );
#endif

	}


	fprintf( stderr, "\nCalling rbsort\n" );
	rbsort( pl_line, line_count );


	for ( i = 0; i <= line_count; i++ )  {

		fprintf( stdout, "%s", pl_line[ i ] );
	}


	return(0);
}
