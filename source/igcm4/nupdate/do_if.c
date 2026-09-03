/* -------------------- FILE: PROC_PL.C ----------------------- */




#include <stdio.h>
#include <errno.h>

#include "defns.h"
#include "structs.h"


extern argument_def   argument;


char                  *proc_ibd();
char                  *rbsearch2();
char                  *get_pl_line();
char                  *get_tmp_line();

extern FILE           *logfile;

extern enum t_boolean   seq_nos;
extern char           master[2];

enum type    { dirv_data, PL_data };
/*
enum type    input_type;
FILE  *directive_input;
*/


int do_if( curr_line, outfile, infile, directive, if_level, if_status, info )

char            *curr_line;
FILE            *outfile;
FILE            *infile;
directive_def   *directive; 
int             if_level;
enum t_boolean    if_status;
info_def        *info;
{
	char  buffer[maxlen+1];
	char  type[maxlen];
	int   retcode;
	int   ret_code;
	enum  t_boolean wrte;
	enum  t_boolean wrten;

	long int      fpos;
	char          *curr_edit_type;
	char          *curr_type;

#ifdef DEBUG
	printf( "\nStarting do_if" ); fflush(stdout);
#endif


	if_level++;

	if ( if_level > max_if_nest )  {
		printf( "\nMax *IF nest exceeded. Aborting at line:\n%s", curr_line);
		return( user_error );
	}


#ifdef DEBUG
	printf( "\nIn do_if: curr_line = %s", curr_line );
	printf( "\nif_level = %d", if_level );
#endif

	if ( if_status == false )  {

		wrte = false;
		wrten = true;    /* Stop all writes in current and sub ifs and calls */
	}
	else  {

		if ( proc_if_def( curr_line, directive->define, directive->num_of_defines)
			== 0 )  {
			wrte = true;
			wrten = true;
#ifdef DEBUG
puts( "\nproc_if_def returned true" );
#endif
		}
		else  {
			wrten = false;
			wrte = false;
#ifdef DEBUG
puts( "\nproc_if_def returned false" );
#endif
		}
	}



/*
 **** Main loop ******
*/

	for ( ;; )  {

     	if ( get_pl_line( buffer, maxlen, infile ) == NULL ) {
			perror( "\nPL End of file\n" );
			prt_error( "\nENDIF missing\n" , buffer, user_error );
		}

		retcode = get_line_type( buffer, type, curr_type, master );

#ifdef DEBUG
	fprintf( stderr, "%s", buffer );
	fprintf( stderr, "\nIn do_if. retcode, curr_type = %d, %s", retcode, 
		curr_type );
	fprintf( stderr, "\nwrte, if_status = %d, %d", wrte, if_status );
#endif

		if ( retcode != ignore )  { 

			if ( wrte == true )  {
				
				if ( strcmp( curr_type, "IF" ) == 0  )  {
					if ( ( ret_code = do_if( buffer, outfile, infile,
					   directive, if_level, wrte, info ) ) != 0 ) 

						return(ret_code);
				}                          /* if *IF */


				if ( strcmp( curr_type, "ELSEIF" ) == 0  )  {
					if ( wrten == true )  {
						wrte = false;
					}
					else  {

						if ( proc_if_def( buffer, directive->define, 
						     directive->num_of_defines ) == 0 )  {
							wrte = true;
							wrten = true;
						}
					}
				}                           /* if ELSEIF */


				if ( strcmp( curr_type, "ELSE" ) == 0 )  {
					if ( wrten == false ) {
						wrte = true;
						wrten = true;
					}
					else
						wrte = false;
				}                           /* if *ELSE */


				if ( strcmp( curr_type, "ENDIF" ) == 0 )  {
					return(0);
				}                           /* if *ENDIF */
			}                              /* wrte == true */
		}                                 /* retcode != ignore */
	}                                    /* for (;;) */



#ifdef DEBUG
	puts( "\nLeaving do_if" ); fflush(stdout);
#endif

	return(user_error);   /* no endif found */
}







char  *get_id( line )
char *line;
{
	static char buffer[maxlen2];
	char  *ptr;

	ptr = line;
	ptr += PL_id_name_start;

	sscanf( ptr, "%s", buffer );

	return( buffer );
}
