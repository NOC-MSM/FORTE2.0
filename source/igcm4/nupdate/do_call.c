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



int do_call( curr_line, call_level, outfile, infile, pl_name, directive, 
             info, id_no )
char            *curr_line;
int             call_level;
FILE            *infile;
FILE            *outfile;
char            *pl_name;
directive_def   *directive;
info_def        *info;
int             *id_no;
{
	FILE  		*comdeck_file;
	char  		name[max_name_len];
	char  		buffer[maxlen+1];
	char  		type[maxlen];
	char  		curr_type[max_name_len];
	char  		comdeck_name[max_name_len];
	int   		retcode;
	int   		ret_code;

	char          *fname;
   char          id_name[max_name_len];
   char          *curr_edit_type;

	int           id2;


#ifdef DEBUG
	printf( "\nStarting do_call" );

	printf( "\nIn do_call: curr_line = %s", curr_line );
	printf( "\ncall_level = %d", call_level );
#endif

	if ( ++call_level > max_call_nest )  {
		printf( "\nMax *CALL nest exceeded. Aborting at line:\n%s", curr_line);
		return( user_error );
	}


/*************************************************************************
  if this is the second level (ie call is from proc_pl then we are 
  reading from a DECK so use file pointer passed from proc_pl.
  else
  check if comdeck is in the temp directory (ie if it has been introduced
  by the mods file.  If not the take from the PL.
**************************************************************************/

	if ( call_level == 1 )  {    /* Top level is a deck */

		comdeck_file = infile;

	}
	else  {

		if ( sscanf( curr_line, "%*s%s", name ) != 1 ) {
			printf( "\n*** Error on line: \"%s\"", curr_line );
			return( user_error );
		}

		sprintf( comdeck_name, comdeckfile, pl_name, name );


		if ( ( comdeck_file = fopen( comdeck_name, "r" ) ) == NULL )  {
			get_outfile_name_2( name, "C", comdeck_name );

			if ( ( comdeck_file = fopen( comdeck_name, "r" ) ) == NULL )  {

				printf( "\n**Error: Unable to find comdeck \"%s\"\n",
					name );

				fprintf( logfile, "\n\n**Error: Unable to find comdeck \"%s\"\n",
					name );

				return( user_error );
			}

		}
	}


/***********************************************************************
   Main loop:
   Read from the comdeck_file and write to the outfile.  If there is
   a line to be edited (ie a IBD affected line) the call do_ibd with
   the name of the temporary line containing the line to be added.
************************************************************************/

	for ( ;; )  {

   	if ( get_pl_line( buffer, maxlen, comdeck_file ) == NULL )  
			break;

		retcode = get_line_type( buffer, type, curr_type, master );


		/* ***** Execute following if affected by an IBD ******* */

		if ( ( fname = proc_ibd( directive, info, comdeck_file, buffer,
			              id_name, curr_edit_type, &id2 ) ) != NULL )  {

			/* **** process if *B or *D **** */
			if ( strcmp( curr_edit_type, "BEFORE" ) == 0  ||
			     strcmp( curr_edit_type, "DELETE" ) == 0 )  {

				if ( ( do_ibd( fname, curr_edit_type, comdeck_file, id2, 
				                    buffer ) ) != 0 )  
					return( user_error );

         	if ( strcmp( curr_edit_type, "BEFORE" ) == 0  )  {
         	   if ( strcmp( curr_type, "CALL" ) == 0 )  {
						if ( do_call( buffer, call_level, comdeck_file, outfile, 
					         pl_name, directive, info, id_no ) != 0 )
						return( user_error );
					}
					else  {
						write_pl_line( buffer, 0, 0, 0, outfile, seq_nos );
					}
				}
			}

			/* ***** process if *I **** */
			if ( strcmp( curr_edit_type, "INSERT" ) == 0  )  {

				if ( strcmp( curr_type, "CALL" ) == 0 )  {

					if ( do_call( buffer, call_level, comdeck_file, outfile,
					              pl_name, directive, info, id_no ) != 0 )
						return( user_error );

						if ( ( do_ibd( fname, curr_edit_type, comdeck_file, 
						                    id2, buffer ) ) != 0 )
						return(user_error);
				}
				else  {

					write_pl_line( buffer, 0, 0, 0, outfile, seq_nos );
				}
			}
		}  /* if IBD */

		/* ***** Execute following not affected by an IBD but is a *CALL ***** */
		else  {

			if ( strcmp( curr_type, "CALL" ) == 0 )  {

				if ( do_call( buffer, call_level, comdeck_file, outfile,
				              pl_name, directive, info, id_no ) != 0 )  {
                  return( user_error );
				}
				/* **** Ordinary line no *CALL or *IBD **** */
				else  {

					write_pl_line( buffer, 0, 0, 0, outfile, seq_nos );
				}
			} /* *CALL */
		}  /* else if *IBD */
	}  /* End of main loop */


	if ( call_level > 1 ) 
		fclose( comdeck_file );
	
	return(0);
}

