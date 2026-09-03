/* -------------------- FILE: PROC_PL.C ----------------------- */




#include <stdio.h>
#include <errno.h>

#include "defns.h"
#include "structs.h"


extern argument_def   argument;
extern char tmpdir[12];   /* Added 14-11-95  Marc Chippindale */


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


proc_pl( directive, pl_name, info, outfile_name )
directive_def *directive; 
char          *pl_name;
info_def      *info;
char          *outfile_name;
{
	int   i;
	char  temp[maxlen];
	char  deckname[maxlen];
	FILE  *curr_deck;
	FILE  *outfile;
	char  buffer[maxlen+1];
	int   retcode;
	int   ret_code;
	char  type[max_name_len];
	char  curr_type[max_name_len];
	int   call_level = 0;
	int   if_level = 0;
	enum  t_boolean  if_status;

	char          *fname;
	char          id_name[max_name_len];
	enum t_boolean  directive_data;
	char          *curr_edit_type;
	char          *id2;
	enum          t_boolean  ibd_flag;
	char          B_buffer[maxlen];
	int           id_no;

	enum type     input_type;
	FILE          *directive_input;
	
	char          tmpfile_name[max_name_len];
	FILE          *tmpfile;



#ifdef DEBUG
puts( "\nstarting proc_pl" ); fflush(stdout);
#endif

	if_status = true;
	id_no  = 0;
	
	sprintf( tmpfile_name, "%s/tmpfile", tmpdir );

	collect_info( directive, pl_name, info );

/* if test added by Paul Burton 9/2/96 for new -D option */
    if (argument.split_output == 0) {    
	  nupopen( outfile, outfile_name, "w", user_error );	
	}

	for ( i = 0; i <= directive->num_of_compiles; i++ )   {

#ifdef DEBUG
		printf( "\nIncluding file: %s", directive->compile[i] );
#endif

		get_outfile_name_2( directive->compile[i], "D", deckname );

		if ( ( curr_deck = fopen( deckname, "r" ) ) == NULL )  {

			sprintf( deckname, deckfile, pl_name, directive->compile[i] );

			if ( ( curr_deck = fopen( deckname, "r" ) ) == NULL )  {

				printf( "\n**Error: Unable to find deck \"%s\"\n", 
					directive->compile[i] );

				fprintf( logfile, "\n\n**Error: Unable to find deck \"%s\"\n", 
					directive->compile[i] );

				return( user_error );
			}
		}

        /* added by Paul Burton 9/2/96 for new -D option */		
	    if (argument.split_output == 1) {
	      nupopen( outfile, directive->compile[i] ,"w", user_error );
	    }

		nupopen( tmpfile, tmpfile_name, "w", user_error );

#ifdef DEBUG
fprintf( stderr, "\nCalling do-call" );
#endif

		if ( ( ret_code = do_call( buffer, call_level, curr_deck, tmpfile,
			  pl_name, directive, info, &id_no ) )
		     != 0 ) {

			printf( "\nError in DECK: %s", directive->compile[i] );
			fprintf( logfile, "\nError in DECK: %s", directive->compile[i] );
			return(ret_code);
		}

		fclose( tmpfile );

		nupopen( tmpfile, tmpfile_name, "r", user_error );
		rewind( tmpfile );

#ifdef DEBUG
fprintf( stderr, "\nCalling do-if" );
#endif

		if ( do_if( buffer, tmpfile, outfile, directive, if_level, if_status, 
		            info )  !=  0  )  {

			printf( "\nError in DECK: %s", directive->compile[i] );
			fprintf( logfile, "\nError in DECK: %s", directive->compile[i] );
			return(ret_code);
		}


		fclose( tmpfile );
		fclose( curr_deck );

/* Added by Paul Burton 9/2/96 for new -D option */		
		if (argument.split_output == 1) { fclose( outfile); }
		
	}
	

/* there's nothing important in buffer */


/* Added by Anette Van der Wal 1/2/00 so as not to close files twice */		
	if (argument.split_output != 1) { fclose( outfile ); }


	puts("\n");

	return(0);
}



/*
   The IBDs array is searched.  The INSERT, BEFORE and DELETE lines are
   examined for the lines and files they modify.  The information is kept
   in separate arrays to speed up the search when the PL is being
   processed.
*/


int collect_info( directive, pl_name, info )
directive_def *directive; 
char          *pl_name;
info_def      *info;
{
	int           retcode;
	int           i, j;
	int           rbsort();
	int           length;
	int           orig_no_of_files;


#ifdef DEBUG
	puts( "\n*** started pl_proc program" );
#endif


/* 
   Allocate space for structure info
*/

	j = -1;

	for ( i = 0; i <= directive->num_of_IBDs; i++ )  {

		if ( strncmp( directive->IBD[i].type, "INSERT", 6 ) == 0 || 
		     strncmp( directive->IBD[i].type, "DELETE", 6 ) == 0 ||
		     strncmp( directive->IBD[i].type, "BEFORE", 6 ) == 0 )  {

			j++;

			Malloc( info->edit_list[j], max_ident_len, system_error );
			Malloc( info->file_list[j], max_ident_len, system_error );
		}
	}



/*
   copy the *I, *B and *D identifiers from the directive structure.
   these will be used for quick checking on each PL line to see if it
   is to be editted.

   Memory is allocated above because Linux crashes the program if it is
   allocated in the loop below.
*/

	info->no_of_edits = -1;
	info->no_of_files = -1;


	for ( i = 0; i <= directive->num_of_IBDs; i++ )  {

		if ( strncmp( directive->IBD[i].type, "INSERT", 6 ) == 0 || 
		     strncmp( directive->IBD[i].type, "DELETE", 6 ) == 0 ||
		     strncmp( directive->IBD[i].type, "BEFORE", 6 ) == 0 )  {


			j = ++info->no_of_edits;

			strcpy( info->edit_list[j], directive->IBD[i].line[0] );


			j = ++info->no_of_files;

			if ( directive->IBD[i].PL_file != NULL )
				strcpy( info->file_list[j], directive->IBD[i].PL_file );
			else  {                            /* The case for quick update */
				info->file_list[j][0] = 251; 
				info->file_list[j][1] = '\0';
			}
		}
	}


	rbsort( info->edit_list, info->no_of_edits );
	rbsort( info->file_list, info->no_of_files );
	orig_no_of_files = info->no_of_files;
	info->no_of_files = remove_repetitions( info->file_list, info->no_of_files );



#ifdef DEBUG
	for ( i = 0; i <= info->no_of_edits; i++ ) 
		printf( "\ni, edit_list[i]: %d, %s", i, info->edit_list[i] );

	puts( "\n" );	

	for ( i = 0; i <= info->no_of_files; i++ )
		printf( "\ni, file_list[i]: %d, %s", i, info->file_list[i] );


	puts( "\n" );	
#endif


	return(0);
}







/* **********  do_call  ************************************* */



int do_call( curr_line, call_level, infile, outfile, pl_name, directive, 
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
	char          temp[maxlen2];

	char          *id2;


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

		if ( (int) strlen(name) > PL_id_name_length )  {
			prt_error( "\n(COM)DECK name too long: ", curr_line, user_error );
		}

		get_outfile_name_2( name, "C", comdeck_name );

#ifdef DEBUG
printf( "\nOpening comdeck_file >%s<\n ", name );
#endif

		if ( ( comdeck_file = fopen( comdeck_name, "r" ) ) == NULL )  {

			sprintf( comdeck_name, comdeckfile, pl_name, name );

			if ( ( comdeck_file = fopen( comdeck_name, "r" ) ) == NULL )  {

				printf( "\n**Warning: Unable to find comdeck \"%s\"\n",
					name );

				fprintf( logfile, "\n\n**Warning: Unable to find comdeck \"%s\"\n",
					name );

/*				return( user_error );  */
				return(0);    /*  Comdeck may be in an false IF DEF loop */
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

   	if ( get_pl_line( buffer, maxlen, comdeck_file ) == NULL )  {
#ifdef DEBUG
fprintf( stderr, "\nbreaking from do_call" );
#endif
			break;
		}
		
#ifdef DEBUG
printf( "\nlevel, buffer = %d, %s", call_level, buffer ); fflush(stdout);
#endif
		retcode = get_line_type( buffer, type, curr_type, master );

		if ( retcode != PL_directve )  *curr_type = '0';

		/* ***** Execute following if affected by an IBD ******* */

		if ( ( fname = proc_ibd( directive, info, comdeck_file, buffer,
			              id_name, &curr_edit_type, &id2 ) ) != NULL )  {

#ifdef DEBUG
printf( "\ncall do_ibd. level, id2 = >%s<, >%d<", call_level, id2 ); 
fflush(stdout);
#endif

			/* **** process if *B or *D **** */
			if ( strcmp( curr_edit_type, "BEFORE" ) == 0  ||
			     strcmp( curr_edit_type, "DELETE" ) == 0 )  {
				if ( do_ibd( fname, curr_edit_type, comdeck_file, outfile, id2, 
				               buffer, call_level, pl_name, directive, info, 
				               id_no, id_name ) != 0 )  
					return( user_error );

#ifdef DEBUG
printf( "\nlevel, leave do_ibd = %d", call_level ); fflush(stdout);
printf( "\ncurr_edit_type, curr_type = >%s<, >%s<", curr_edit_type, curr_type );
fflush( stdout );
#endif

         	if ( strcmp( curr_edit_type, "BEFORE" ) == 0  )  {
         	   if ( strcmp( curr_type, "CALL" ) == 0 )  {
						if ( do_call( buffer, call_level, comdeck_file, outfile, 
					         pl_name, directive, info, id_no ) != 0 )
						return( user_error );
					}
					else  {
						write_pl_line( buffer, 0, 0, 0, outfile, true );
					}
				}
			}

			/* ***** process if *I **** */
			if ( strcmp( curr_edit_type, "INSERT" ) == 0  )  {

				if ( strcmp( curr_type, "CALL" ) == 0 )  {

					if ( do_call( buffer, call_level, comdeck_file, outfile,
					              pl_name, directive, info, id_no ) != 0 )
						return( user_error );

					if ( do_ibd( fname, curr_edit_type, comdeck_file, outfile, 
				             id2, buffer, call_level, pl_name, directive, info, 
				             id_no, id_name ) != 0 )  
						return( user_error );
				}
				else  {

					write_pl_line( buffer, 0, 0, 0, outfile, true );
					if ( do_ibd( fname, curr_edit_type, comdeck_file, outfile, 
				             id2, buffer, call_level, pl_name, directive, info, 
				             id_no, id_name ) != 0 )  
						return( user_error );
				}
			}
		}  /* if IBD */

		/* ***** Execute following not affected by an IBD but is a *CALL ***** */
		else  {

#ifdef DEBUG
	fprintf( stderr, "\ndoing else *CALL" );
#endif
			if ( strcmp( curr_type, "CALL" ) == 0 )  {

				if ( do_call( buffer, call_level, comdeck_file, outfile,
				              pl_name, directive, info, id_no ) != 0 )  {
                  return( user_error );
				}
			}
			/* **** Ordinary line no *CALL or *IBD **** */
			else  {

				write_pl_line( buffer, 0, 0, 0, outfile, true );

			} /* *CALL */
		}  /* else if *IBD */
	}  /* End of main loop */


	if ( call_level > 1 ) 
		fclose( comdeck_file );

#ifdef DEBUG
fprintf( stderr, "\nLeaving do_call. level = %d", call_level );	
#endif

	return(0);
}










/* ***************************  do_if  ****************************** */


int do_if( curr_line, infile, outfile, directive, if_level, if_status, info )

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
	char          curr_type[maxlen];
	enum t_boolean  directive_data =  false;

#ifdef DEBUG
	printf( "\nStarting do_if" ); fflush(stdout);
#endif


	if_level++;

	if ( if_level > max_if_nest )  {
		printf( "\nMax *IF nest exceeded. Aborting at line:\n%s", curr_line);
		return( user_error );
	}

	if ( if_level == 1 )  {
		if_status = true;
		wrte  = true;
		wrten = true;
	}


#ifdef DEBUG
	printf( "\nIn do_if: curr_line = %s", curr_line );
	printf( "\nif_level = %d", if_level );
#endif

	if ( if_level > 1 )  {

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
	}



/*
 **** Main loop ******
*/

	for ( ;; )  {

     	if ( get_pl_line( buffer, maxlen, infile ) == NULL ) {
			if ( if_level == 1 )  return(0);
			prt_error( "\nENDIF missing\n" , buffer, user_error );
		}

		directive_data = false;

		retcode = get_line_type( buffer, type, curr_type, master );

#ifdef DEBUG
	fprintf( stderr, "%s", buffer );
	fprintf( stderr, "\nIn do_if. retcode, curr_type = %d, %s", retcode, 
		curr_type );
	fprintf( stderr, "\nwrte, if_status = %d, %d", wrte, if_status );
#endif

		if ( retcode == PL_directve )  { 

			if ( strcmp( curr_type, "IF" ) == 0  )  {
				if ( ( ret_code = do_if( buffer, infile, outfile,
				   directive, if_level, wrte, info ) ) != 0 ) 

					return(ret_code);  /* error return code */
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
		}
		else  {

			if ( wrte == true  &&  if_status == true )
				write_pl_line( buffer, 0, 0, 0, outfile, seq_nos );
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

