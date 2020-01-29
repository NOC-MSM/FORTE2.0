/* -------------------- FILE: PROC_DIR.C ----------------------- */




#include "defns.h"
#include "structs.h"

#ifdef MSDOS
#	include <dir.h>
#endif


char *rbsearch2();

extern  argument_def   argument;
extern  enum t_boolean   seq_nos;
extern char tmpdir[12];   /* Added 14-11-95  Marc Chippindale */


extern FILE  *logfile;

char         *get_drv_line();
char         *get_xref_line();



/*


main()
{
	int       retcode;
	int       proc_dir();
	int       i;
	char      directives_file_name[maxlen];
	char      pl_name[maxlen];
	char      outfile_name[maxlen];

	directive_def  directive;
	info_def       info;

#ifdef DEBUG
	puts( "started program" );
#endif

	
	strcpy( directives_file_name, "input" );
	strcpy( pl_name, "mylib" );
	strcpy( outfile_name, "output" );


	if ( ( retcode = proc_dir( directives_file_name, &directive ) ) != 0 )
		exit( retcode );

	if ( ( retcode = get_deck_names( &directive, pl_name ) ) != 0 )
		exit( retcode );

	if ( ( retcode = resolve_comdeck_calls( &directive, pl_name ) ) != 0 )
		exit(retcode );



#ifdef DEBUG

	printf( "\n\nident = %s", directive.ident );

	printf( "\n\nno_compiles = %d", directive.num_of_compiles );
	for ( i = 0; i <= directive.num_of_compiles; i++ )  {
		printf( "\n%d = %s", i, directive.compile[i] );
	}


	printf( "\n\nno_defines = %d", directive.num_of_defines );
	for ( i = 0; i <= directive.num_of_defines; i++ )  {
		printf( "\n%d = %s", i, directive.define[i] );
	}


	printf( "\nnum_of_comdecks = %d", directive.num_of_comdecks );
   for ( i = 0; i <= directive.num_of_comdecks; i++ )  {
      printf( "\n%d = %s", i, directive.comdeck[i] );
   }

	printf( "\nnum_of_decks = %d", directive.num_of_decks );
   for ( i = 0; i <= directive.num_of_decks; i++ )  {
      printf( "\n%d = %s", i, directive.deck[i] );
   }





	puts( "\n------------------------------------------" );
	printf( "\n\nno_IBDs = %d", directive.num_of_IBDs );
	for ( i = 0; i <= directive.num_of_IBDs; i++ )  {
		puts( "\n" );
		printf( "\n%d) type = %s", i, directive.IBD[i].type );
		printf( "\nident, declare = %s, %s", 
		        directive.IBD[i].ident, directive.IBD[i].declare );
		printf( "\nfile = %s", directive.IBD[i].file );
		printf( "\nline1,2 = %s, %s", directive.IBD[i].line[0],
	           directive.IBD[i].line[1] );
		printf( "\ndeck_type, PL_file = %s, %s", directive.IBD[i].deck_type,
		                                         directive.IBD[i].PL_file );
	}

#endif


	strcpy( directive.master, "*" );

	proc_pl( &directive, pl_name, &info, outfile_name );

	printf( "\nEnded program in main\n" );
	return(0);
}


*/


/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 19 Apr 1993.

   Process the directives file.  Save *I, *B and *D data in temporary files.
   Also save the *COMDECK and *DECK data in separate (temporary) file.
   Save other directives in variables in memory.

*/


int proc_dir( directives_file_name, directive )
char *directives_file_name;
directive_def *directive;
{
	FILE *directives_file;
	FILE *output_file;
	char output_file_name[ max_filename_len ];
	char buffer[ maxlen ];
	char temp[ maxlen ];
	enum t_boolean  writing_to_file;
	int retcode;
	int ret_code;
	char type[maxlen] = "\0";
	char curr_type[maxlen] = "\0";
	int n;
	int get_line_type();
	int deck_ident_number = 1;   /* not 0 because headers missing */
	char deckname[ max_name_len ];
	int  itemp;

	enum t_boolean  file_open;
	FILE  *outfile;
	char  outfile_name[ maxlen ];
	char  tmpbuf[maxlen];


#ifdef DEBUG
	puts( "In proc_dir" ); fflush( stdout );
#endif



	directive->num_of_IBDs = -1;
	directive->num_of_calls = -1;
	directive->num_of_decks = -1;
	directive->num_of_comdecks = -1;

	strcpy( (*directive).ident, "$_none" );
	strcpy( (*directive).master, "*" );   /*   * is the default   */

	nupopen( directives_file, directives_file_name, "r", user_error );

	rewind( directives_file );

/*
	Main loop. Process the directives file
*/

	file_open = false;   /* Open file for DECK or COMDECK commands */


	while( get_drv_line( buffer, maxlen, directives_file )  !=  NULL )  {

		retcode = get_line_type( buffer, type, curr_type, directive->master );

#ifdef DEBUG
		fprintf( stderr, "\nProc_dir: %s", buffer );
		fprintf( stderr, "\nretcode, type =  %d, %s", retcode, type );
#endif

		if ( retcode == user_error  ||  retcode == system_error )
			return( retcode );

		if ( sscanf( buffer, "%s", tmpbuf ) <= 0  &&  file_open == false )
			retcode = ignore;


		if ( retcode != ignore )  {

			if ( retcode == directve )  {

				if ( file_open == true )  fclose( outfile );
				file_open = false;


				if ( strcmp( type, "INSERT"  ) == 0  ||
				     strcmp( type, "DELETE"  ) == 0  ||
					  strcmp( type, "BEFORE"  ) == 0 )  {

            	n = ++directive->num_of_IBDs;

					if ( ( ret_code = process_IBD( buffer,
					                  &(directive->IBD[n]), type ) ) != 0 )
               	return( ret_code );
               else  {
               	nupopen( outfile, directive->IBD[n].file, "w",
						       system_error ); 
						file_open = true;

						if ( strncmp( (*directive).ident, "$_none", 6 ) == 0 )  {
							prt_error( "\nError: No identity for line: ", buffer, 
						              user_error );
						}
/*						fprintf( outfile, "%s\n", (*directive).ident );  */
               }
            }



				if ( strcmp( type, "DECLARE" ) == 0  ||
				     strcmp( type, "IDENT"   ) == 0 )  {

            	n = ++directive->num_of_IBDs;

					if ( ( ret_code = process_IBD( buffer,
					                  &(directive->IBD[n]), type ) ) != 0 )
               	return( ret_code );
				}



				if ( strcmp( type, "COMDECK" ) == 0  ||
				     strcmp( type, "DECK" ) == 0 )  {

               if ( ( ret_code = get_outfile_name( buffer, type,
					                  outfile_name ) )  != 0 )
						return( ret_code );
					else  {
						nupopen( outfile, outfile_name, "w", user_error );
						file_open = true;
						sscanf( buffer, "%*s%s", deckname );
	
						if ( (int) strlen( deckname ) > PL_id_name_length )  {
							prt_error( "\n**Error (COM)DECK name too long: ",
								buffer, user_error );
						}

						if ( strncmp( type, "D", 1 ) == 0 )  {
							itemp = ++directive->num_of_decks;
							Malloc_and_copy( directive->deck[itemp], deckname );
						}
						if ( strncmp( type, "C", 1 ) == 0 )  {
							itemp = ++directive->num_of_comdecks;
							Malloc_and_copy( directive->comdeck[itemp], deckname );
						}
						deck_ident_number = 1;  /* != 0 because headers not saved */
					}
				}

				if ( strcmp( type, "COMPILE" ) == 0 )  {
					if ( ( ret_code = save_C_D_directives(
					                  buffer,
					   	            directive->compile,
								         &directive->num_of_compiles,
								         type ) )  !=  0 )
						return( ret_code );
				}

				if ( strcmp( type, "DEFINE" ) == 0 )  {
					if ( ( ret_code = save_C_D_directives(
					                     buffer,
					                     directive->define,
												&directive->num_of_defines,
												type ) )  !=  0 )
						return( ret_code );
				}


				if ( strcmp( type, "IDENT" ) == 0 )  {
					if ( ( ret_code = save_ident(
					                     buffer,
					                     (*directive).ident ) )  !=  0 )
						return( ret_code );
				}

			}


			if ( retcode == source_code || retcode == PL_directve )  {

				if ( file_open == true )  {
/*
					prt_error( "\nError: Line must be in a control block",
					           buffer, user_error );
*/

					if ( strcmp( type, "COMDECK" ) == 0  ||
	           	   strcmp( type, "DECK" ) == 0 )  {

						deck_ident_number++;
						if ( ( ret_code = write_tmp_line(
							               	buffer,
						   	               deckname,
						      	         	PL_id_separator,
						         	      	deck_ident_number,
						            	   	outfile, true,
 													directive->master ) )  !=  0  )
							return( ret_code );
					}


					if ( strcmp( type, "INSERT" ) == 0  ||
					     strcmp( type, "DELETE" ) == 0  ||
					     strcmp( type, "BEFORE" ) == 0 )  {
						if ( ( retcode = write_tmp_line( buffer, 0, 0, 0, outfile, 
							              	false, directive->master ) ) != 0 )  
							return( user_error );
					}
				}
				else  {
#ifdef SAVE_LOG
					fprintf( logfile, "\nIgnoring line: %s", buffer );
#endif
				}
			}
		}
   }



	if ( file_open == true )  fclose( outfile );
	fclose(  directives_file );

	if ( ( ret_code = set_decl_ident( directive ) != 0 ) )  return( ret_code );

	copy_deck_to_compile( directive );



	rbsort( directive->compile, directive->num_of_compiles );
	rbsort( directive->define, directive->num_of_defines );

	return(0);
}






/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 27 Dec 1992.

	Save the COMPILE and DEFINE directives in arrays.  Compile directives
	need to have memory allocated (using malloc).
*/


int save_C_D_directives( instring, directive_args, directive_num, type )
char *instring;
char *directive_args[];
int  *directive_num;
char *type;
{
	char temp[ maxlen ];
	int n;
	char *getarg();
	char *ptr;

/*
	Error check only. Make sure *directive has an argument.
*/
	if ( sscanf( instring, "%*s%s", temp )  !=  1 )  {
		prt_error( "Error in line:", instring, user_error );
	}

/*
	Remove commas, which separate arguments.
*/
	strcpy( temp, instring );
	subst( instring, ",\0", " \0" );

/*
	Remove first entry (*directive).
*/
	ptr = getarg( instring, temp );

/*
	Get arguments.
*/

	while ( ( ptr = getarg( ptr, temp ) )  !=  NULL )  {

		n = ++(*directive_num);

		if ( strcmp( type, "DEFINE" ) == 0 )  {
			Malloc( directive_args[ n ], max_name_len, system_error );
		}
		else if ( strcmp( type, "COMPILE" ) == 0 )  {
			Malloc( directive_args[ n ], strlen( temp ) + 1, system_error );
		}

		strcpy( directive_args[ n ], temp );
	}

	return(0);
}





/*
    Author: Ramesh Krishna  (Met Office - Central Computing).
    Last Modified: 25 Dec 1993.

    Save the IDENTity to variable directive_id read from string instring.
*/


save_ident( instring, directive_id )
char *instring;
char *directive_id;
{
	char temp[ max_name_len ];

	strcpy( temp, directive_id );

	if ( sscanf( instring, "%*s%s", directive_id ) != 1 )  {
		prt_error( "Error reading IDENT in line:", instring, user_error );
	}

	if ( strcmp( temp, "$_none" )  !=  0 )  {
		puts( "\n***Warning: more than one ID statement in directives file" );
		printf( "Using \"%s\"\n", directive_id );
	}

	return(0);
}



/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 27 Dec 1992.

   Get name of file to save *DECK and *COMDECK lines appearing in the
   directives file.
*/

int get_outfile_name( instring, type, outfile_name )
char *instring, *type, *outfile_name;
{
	char filename[ maxlen ];

	if ( sscanf( instring, "%*s%s", filename )  !=  1 )
		prt_error( "\nError: name missing in line:", instring, user_error );

	if ( (int) strlen( filename ) > PL_id_name_length )  
	 	filename[PL_id_name_length] = '\0';

	if ( strncmp( type, "C", 1 ) == 0 )
		sprintf( outfile_name, "%s/%s.cdk.tmp", tmpdir, filename );

	if ( strncmp( type, "D", 1 ) == 0 )
		sprintf( outfile_name, "%s/%s.dk.tmp", tmpdir, filename );

	return(0);
}




/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 14 Jun 1993.

   Similar to get_outfile_name but input is the name of the deck or comdeck.
*/


int get_outfile_name_2( instring, type, outfile_name )
char *instring, *type, *outfile_name;
{
	char filename[ maxlen ];


	if ( strncmp( type, "C", 1 ) == 0 )
		sprintf( outfile_name, "%s/%s.cdk.tmp", tmpdir, instring );

	if ( strncmp( type, "D", 1 ) == 0 )
		sprintf( outfile_name, "%s/%s.dk.tmp", tmpdir, instring );

	return(0);
}






/*
    Author: Ramesh Krishna  (Met Office - Central Computing).

    Last Modified: 27 Dec 1992.

    Save the *I, *B and *D directives in variables and the following
    insert text in separate files.
*/


int process_IBD( buffer, IBD, type )
char    *buffer;
IBD_def *IBD;
char    *type;
{
   char temp_buffer[ maxlen ];
   char temp[ maxlen ];
   char temp2[ maxlen ];
   char first_arg[ maxlen ];
   char second_arg[ maxlen ];
   int  length;
   char input[20];
   char *ptr;
   char *tmpname();

   if ( strcmp( type, "INSERT"  ) != 0  &&
        strcmp( type, "BEFORE"  ) != 0  &&
        strcmp( type, "DELETE"  ) != 0  &&
	     strcmp( type, "DECLARE" ) != 0  &&
	     strcmp( type, "IDENT"   ) != 0 )
   	prt_error( "\nSystem error in routine process_IBD. type = ",
   		type, system_error );


   IBD->line[0] = NULL;   /* There may not be a second line id */
   IBD->line[1] = NULL;   /* There may not be a second line id */

   IBD->deck_type = NULL;
   IBD->PL_file   = NULL;

	IBD->executed  = false;


	strcpy( temp_buffer, buffer );
	strcpy( first_arg, "\0" );
	strcpy( second_arg, "\0" );



	if ( strcmp( type, "DECLARE" ) == 0 )  {

		if ( sscanf( temp_buffer, "%*s%s", first_arg ) != 1 )
			prt_error( "\nError: Name missing in DECLARE stetement.\n",
			           buffer, user_error );

		Malloc( IBD->type, strlen( type ) + 1, system_error );
		strcpy( IBD->type, type );
		Malloc( IBD->declare, strlen( first_arg ) + 1, system_error );
		strcpy( IBD->declare, first_arg );
		return(0);
	}


	if ( strcmp( type, "IDENT"   ) == 0 )  {

		if ( sscanf( temp_buffer, "%*s%s", first_arg ) != 1 )
			prt_error( "\nError: Name missing in IDENT stetement.\n",
			           buffer, user_error );

		Malloc( IBD->type, strlen( type ) + 1, system_error );
		strcpy( IBD->type, type );
		Malloc( IBD->ident, strlen( first_arg ) + 1, system_error );
		strcpy( IBD->ident, first_arg );
		return(0);
	}



/*  Do the INSERT, DELETE of BEFORE statements  */

	if ( strcmp( type, "INSERT" ) == 0  || strcmp( type, "BEFORE" ) == 0  ) {

		if ( sscanf( buffer, "%*s%s", first_arg )  !=  1 )
			prt_error( "Error: Missing name in line:", buffer, user_error );

		if ( strstr( first_arg, "." ) == NULL )
			prt_error( "Error in argument to *I or *B.",
                    buffer, user_error );

		Malloc( IBD->type, strlen( type ) + 1, system_error );
		strcpy( IBD->type, type );

		Malloc( IBD->line[0], strlen( first_arg ) + 1, system_error );
		strcpy( IBD->line[0], first_arg );

	}


	if ( strcmp( type, "DELETE" ) == 0 )  {

   	strcpy( temp_buffer, buffer );
      subst( temp_buffer, ",\0", " \0" );

		if ( sscanf( temp_buffer, "%*s%s%s", first_arg, second_arg )  ==  0 )
			prt_error( "Error: Missing args in line:", buffer, user_error );

		if ( strstr( first_arg, "." ) == NULL )
			prt_error( "Error in argument to *I or *B.",
                    buffer, user_error );

		Malloc( IBD->type, strlen( type ) + 1, system_error );
		strcpy( IBD->type, type );

		Malloc( IBD->line[0], strlen( first_arg ) + 1, system_error );
		strcpy( IBD->line[0], first_arg );

      if ( (int) strlen( second_arg ) > 0 )  {

			/* If id name is not in the 2nd arg take it from the 1st arg */

			if ( strstr( second_arg, "." ) == NULL )  {

				if ( sscanf( first_arg, "%[^.]", temp ) != 1 )
					prt_error( "\nError in 1st argument in line:", buffer,
					           user_error );

         	sprintf( temp2, "%s.%s", temp, second_arg );
         	strcpy( second_arg, temp2 );
      	}


			Malloc( IBD->line[1], strlen( second_arg ) + 1, system_error );
			strcpy( IBD->line[1], second_arg );
      }
		else
			IBD->line[1] = NULL;
	}


	if ( ( ptr = (char *) tmpname() ) == NULL )  {
		printf( "\nPIBD.tmpnam unable to generate any more temp file names\n" );
		return(system_error);
	}

   Malloc( IBD->file, strlen( ptr ) + 6, system_error );

   strcpy( IBD->file, ptr );


#ifdef DEBUG
	printf( "\ntemp = %s", temp );
   printf( "\nptr = %p", ptr );
   printf( "\ntemp filename = %s", IBD->file );
   printf( "\ntemp filename address = %p", IBD->file );

   puts( "\n---------------------------------------------------------" );
#endif


   return(0);
}





	
/*
    Author: Ramesh Krishna  (Met Office - Central Computing).

    Last Modified: 23 Apr 1993.

    Adjust the declare and ident variables in the IBD structure to record
    the current declare name and identity.

    The declare variable may be set to NULL if no DECLARE statement is in 
    effect.
*/


int set_decl_ident( directive )
directive_def *directive;
{
	int i;
	char *ident;
	char *declare;

	int num_of_IBDs;


	num_of_IBDs = directive->num_of_IBDs;


/* 
   IDENT should be set as the first directive. If not, set ident in first 
   directive to NONE (should normally flag an error).
*/

	if ( strcmp( directive->IBD[0].type, "IDENT" ) != 0 )  {
/*
		printf( "\ntype = %s\n", directive->IBD[0].type  );
		prt_error( "\nError: IDENT statement missing at start of directives file",
		     "\nAborting\n", user_error );
*/
		Malloc_and_copy( directive->IBD[0].ident, "NONE\0" );
	}


	directive->IBD[0].declare = NULL;


	for ( i = 0; i <= num_of_IBDs; i++ )  {

		if ( strcmp( directive->IBD[i].type, "IDENT" ) == 0 ) {  /* IDENT */
			directive->IBD[i].declare = NULL;
		}                                                        /* DECLARE */
		else if ( strcmp( directive->IBD[i].type, "DECLARE" ) == 0 ) { 
         directive->IBD[i].ident   = directive->IBD[i-1].ident;
		}
		else  {                                    /* Not DECLARE or IDENT */
			directive->IBD[i].declare = directive->IBD[i-1].declare;
			directive->IBD[i].ident   = directive->IBD[i-1].ident;
		}
	}

	return(0);
}






/*
    Author: Ramesh Krishna  (Met Office - Central Computing).

    Last Modified: 27 Apr 1993.

    This routine saves the deck name corresponding to each directive of the
    form Insert, Before or Delete. Save is to the "directive" structure member
    directive.IBD.PL_file.
*/


int get_deck_names( directive, pl_name )
directive_def *directive;
char *pl_name;
{
	FILE   *index_file;
	char   index_file_name[ max_name_len ];
	char   *index[ max_pl_lines ];
	char   buffer[ maxlen ];
	char   temp1[ maxlen ];
	char   temp2[ maxlen ];
	int    no_of_indices = -1;
	int    i;
	char   *ptr;
	char   deck_type[5];
	int    num_of_IBDs;



#ifdef DEBUG
	puts( "\nIn get_deck_names\n" ); fflush( stdout );
#endif

	num_of_IBDs = directive->num_of_IBDs;

/*  
   If we are using quick update we don't need to worry about checking for
   dependencies.
*/

	if ( *argument.mode == 'q' )  return(0);


	sprintf( index_file_name, "%s/index", pl_name );

	nupopen( index_file, index_file_name, "r", user_error );

	rewind( index_file );


	while ( get_idx_line( buffer, maxlen, index_file ) != 0 )  {
		Malloc( index[++no_of_indices], strlen( buffer ) + 1, system_error );
		strcpy( index[no_of_indices], buffer );
	}


	for ( i = 0; i <= num_of_IBDs; i++ )  {

		if ( directive->IBD[i].line[0] != NULL )  {
			ptr = (char *) rbsearch2( index, no_of_indices, 
			                          directive->IBD[i].line[0] );
			if ( ptr == NULL )  {
#ifdef SAVE_LOG
				fprintf( logfile, "\nWarning: line(s): %s %s not found in PL",
				        directive->IBD[i].line[0], directive->IBD[i].line[1] );
#endif
				printf( "\nWarning: line(s): %s %s not found in PL",
				        directive->IBD[i].line[0], directive->IBD[i].line[1] );
				directive->IBD[i].PL_file = NULL;     /* these errors are */
				directive->IBD[i].deck_type = NULL;   /* reported later */
			}
			else  {
				sscanf( ptr, "%*s%s%s", temp1, temp2 );
				Malloc( directive->IBD[i].deck_type, strlen(temp1)+1, system_error );
				Malloc( directive->IBD[i].PL_file, strlen(temp2)+1, system_error );
				strcpy( directive->IBD[i].deck_type, temp1 );
				strcpy( directive->IBD[i].PL_file, temp2 );
			}	
		}
		else  {
			directive->IBD[i].deck_type = NULL;
			directive->IBD[i].PL_file   = NULL;
		}
	}



	fclose( index_file );
	for ( i = 0; i <= no_of_indices; i++ )  free( index[i] );

	return(0);
}






/*
    Author: Ramesh Krishna  (Met Office - Central Computing).

    Last Modified: 27 Apr 1993.

    This routine scans the *I, *B, *D  directives and finds the COMDECKS
    altered.  Then the dependent DECKs are found and added to the 
    directive.compile array.
*/



int resolve_comdeck_calls( directive, pl_name )
directive_def *directive;
char *pl_name;
{
	char *comdeck_name;
	char *deck_name;
	char buffer[maxlen3];
	char temp[maxlen2];
	char xref_name[max_name_len];
	FILE *xref_file;
	int  num_of_IBDs;
	int  i, j;
	int  num_of_comdecks = -1;
	char *decks[max_decks];
	char *deck_list;
	char *comdecks[max_comdecks];
	int  num_decks;
	int  start = 0;
	enum t_boolean status_flag;
	char *ptr;
	FILE *fp;

	char *comdeck_xref[max_comdecks];
	int  num_of_xrefs = -1;


#ifdef DEBUG
	puts( "\nIn resolve_comdeck_calls\n" ); fflush( stdout );
#endif


	if ( *argument.mode == 'n' || *argument.mode == 'f' )   {    /* normal update */
                                                              /* or full update */
		num_of_IBDs = directive->num_of_IBDs;

		sprintf( xref_name, "%s/xref", pl_name );
		nupopen( xref_file, xref_name, "r", user_error );
		rewind( xref_file );


		while( get_xref_line( buffer, maxlen3-1, xref_file ) != NULL )  {
			num_of_xrefs++;
			Malloc_and_copy( comdeck_xref[num_of_xrefs], buffer );
		}

		fclose( xref_file );


		for ( i = 0; i <= num_of_IBDs; i++ )  {
			if ( strncmp( directive->IBD[i].deck_type, "C", 1 ) == 0 )  { 
	
				comdeck_name = directive->IBD[i].PL_file;
				deck_list = rbsearch2( comdeck_xref, num_of_xrefs, comdeck_name );

				if ( ( ptr = strstr( deck_list, " \0" ) ) != NULL )  {
					num_decks = process_list( ptr, &decks[start], max_decks-1 );
					start += num_decks + 1;
				}

			}
		}

		start--;


/*
   XXX - repeat above loop in form   
	for ( i = 0; i <= directive->num_of_comdecks; i++ )  {
		deck_list = rbsearch2( comdeck_xref, num_of_xrefs, 
		                       directive->comdeck[i] );
      if ( ( ptr = strstr( deck_list, " \0" ) ) != NULL )  {
         num_decks = process_list( ptr, &decks[start], max_decks-1 );
         start += num_decks + 1;
      }
	}

	start--;
   XXX to include comdecks defined in the directives file.
*/


		for ( i = 0; i <= num_of_IBDs; i++ )  {
			if ( strncmp( directive->IBD[i].deck_type, "D", 1 ) == 0 )  { 

				deck_name = directive->IBD[i].PL_file;

				if ( rbsearch2( comdeck_xref, num_of_xrefs, deck_name ) != NULL ) {
					start++;
					Malloc_and_copy( decks[start], deck_name );
				}
				else
					printf( "\nWarning: Unable to find DECK: %s", deck_name );
			}
		}


#ifdef DEBUG
printf( "\nIn resolve_comdeck_calls: start = %d", start );
for ( i = 0; i <= start; i++ )  printf( "\ni, deck = %d, %s", i, decks[i] );
#endif

		rbsort( decks, start );


		num_decks = remove_repetitions( decks, start ); 

		for ( j = 0; j <= num_decks; j++ ) {
			i = ++directive->num_of_compiles;
			Malloc_and_copy( directive->compile[i], decks[j] );
		}

		for ( i = 0; i <= num_of_xrefs; i++ )  free( comdeck_xref[i] );
/*
	Temp fix.
		for ( i = 0; i <= start; i++ )         free( decks[i] );
*/

	}   /* normal update */




	if ( *argument.mode == 'f' )   {      /* full update. Read names from deck.lst */

		sprintf( buffer, "%s/deck.lst", pl_name );
		nupopen( fp, buffer, "r", user_error );

		while( fgetz( buffer, maxlen, fp ) != 0 )  {
			i = ++directive->num_of_compiles;
			sscanf( buffer, "%s", temp );
			Malloc_and_copy( directive->compile[i], temp );
		}

		fclose( fp );

	}    /* full update */
		



	rbsort( directive->compile, directive->num_of_compiles );

	directive->num_of_compiles = 
	remove_repetitions( directive->compile, directive->num_of_compiles );



	return(0);
	
}






/*
    Author: Ramesh Krishna  (Met Office - Central Computing).

    Last Modified: 14 Jun 1993.

    Copy the members of directive.deck to directive.compile for inclusion 
    of directive_file defined decks during reading of *C decks into the
    output file.
*/

int copy_deck_to_compile( directive )
directive_def  *directive;
{
	int  i;
	int  j;


	for ( i = 0; i <= directive->num_of_decks; i++ )  {
		j = ++directive->num_of_compiles;
		Malloc_and_copy( directive->compile[j], directive->deck[i] );
	}

	return(0);
}
