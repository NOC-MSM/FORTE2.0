/* ------------------------ FILE: MKLIB.C ------------------------ */






#include "defns.h"
#include "structs.h"

cdeck_def  cdeck[max_decks];
char tmpdir[12];   /* Added 14-11-95  Marc Chippindale */



FILE *pl;  /* program library handle */
FILE *pl_index;  /* index file containing sorted pl indices */

extern char *get_tempfile();

#define logfile_name "mklib.log"
FILE *logfile;

int   tmpfile_num = 0;



/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 26 Dec 1992.
*/


main( argc, argv )
int argc; char *argv[];
{
   int ret_code;


   sprintf( tmpdir, "_up%d.tmp", getpid() );  /* Added 15-11-95  Marc Chippindale */

   if ( argc != 3 )  {
      printf( "\nUsage: mklib library_name input_file_name\n" );
      exit( user_error );
   }

#ifdef SAVE_LOG
   nupopen( logfile, logfile_name, "w", system_error );
#endif


	puts( "\nDoing create_library" ); fflush( stdout );

   if ( ( ret_code = create_library( argv[2], argv[1] ) ) != 0 )
      exit(ret_code );

	puts( "\nCalling create_idx" ); fflush( stdout );

   if ( ( ret_code = create_idx( argv[2], argv[1] ) ) != 0 )
      exit(ret_code );


	puts( "\nCalling create_xref" ); fflush( stdout );

   if ( ( ret_code = create_xref( argv[2], argv[1] ) ) != 0 )
      exit(ret_code );

   write_master( argv[1] );

#ifdef SAVE_LOG
   fputs( "\n\nFinished mklib in main\n\n", logfile );
   fclose(logfile);
#endif


	printf( "\nLeaving in main\n" ); fflush( stdout );

   exit(0);

}






/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 25 Dec 1992.

   Standard update instructions are read from an input (directives) file
   and these are used to create a new library (i.e. a directory)
   containing the  DECKs and COMDECKs in files with the same names as the
   DECKs and COMDECKs (but with suffixes .dk and .cdk resp.

   input_file_name:  name of input directives file.
   library_name   :  name of library (or directory) to install files.
*/




int create_library( input_file_name, library_name )
char *input_file_name, *library_name;
{
   FILE  *input_file_handle, *file_handle;
   int   ret_code;
   char  file_name[maxlen2];
   char  temp[maxlen];
   char  buffer[maxlen2];
   enum  t_boolean wrte;
   int   dk;
   int   cdk;
   int   counter;
	enum  t_boolean file_open = false;
	FILE  *comdeck_list, *deck_list;

	char  *dkname[max_decks];
	int   dkname_no = -1;
	char  *cdkname[max_comdecks];
	int   cdkname_no = -1;

	int   i, n;



   nupopen( input_file_handle, input_file_name, "r", user_error );

   if ( ( ret_code = make_directory( library_name, "create" ) ) != 0 )
      return( ret_code );

	sprintf( buffer, comdecklist, library_name );
	nupopen( comdeck_list, buffer, "w", user_error );

	sprintf( buffer, decklist, library_name );
	nupopen( deck_list, buffer, "w", user_error );

/*
   Now save the DECKs and COMDECKs in separate files in the library
   (also known as a directory).  File names are as described above.
*/

   rewind( input_file_handle );
   wrte = false;


	file_open = false;

   while ( fgets( buffer, maxlen, input_file_handle ) != NULL )  {
/*
		puts( buffer );
*/
		wrte = true;

      if ( ( dk = strncmp( buffer, "*DECK", 5 ) ) == 0  ||
           ( cdk = strncmp( buffer, "*COMDECK", 8 ) ) == 0 ||
	   ( dk = strncmp( buffer, "*DK", 3 ) ) == 0 ||
	   ( cdk = strncmp( buffer, "*CDK", 4 ) ) == 0 )  {

         if ( sscanf( buffer, "%*s%s", temp ) != 1 )  {
            puts( buffer );
            printf( "\nError, DECK or COMDECK name missing\n" );
            return( user_error );
         }


         if ( dk == 0 )  {
            sprintf( file_name, deckfile, library_name, temp );
/* 			fprintf( deck_list, "%s\n", temp );  */

				dkname_no++;
				Malloc_and_copy( dkname[dkname_no], temp );
			}
         else  {
            sprintf( file_name, comdeckfile, library_name, temp );
/*				fprintf( comdeck_list, "%s\n", temp );  */
				cdkname_no++;
				Malloc_and_copy( cdkname[cdkname_no], temp );
			}

#ifdef SAVE_LOG
			fprintf( logfile, "\n(com)deckfile = %s", file_name );
#endif

			if ( file_open == true )  {
				fclose( file_handle );
				file_open = false;
			}

         nupopen( file_handle, file_name, "w", user_error );
			file_open = true;

         wrte = false;
         counter = 0;
      }


      if ( wrte == true )  {
			counter++;
			if ( *buffer == '*' )  *buffer = Master_char;
			fputs( buffer, file_handle );
		}

   }


	rbsort( dkname, dkname_no );
	n = dkname_no;
	remove_repetitions( dkname, dkname_no );

	if ( n != dkname_no )
		puts( "\n*** Warning: DECK names have been repeated\n" );


	rbsort( cdkname, cdkname_no );
	n = cdkname_no;
	remove_repetitions( cdkname, cdkname_no );

	if ( n != cdkname_no )
		puts( "\n*** Warning: COMDECK names have been repeated\n" );




	for ( i = 0; i <= dkname_no; i++ )  {
		fprintf( deck_list, "%s\n", dkname[i] );
		free( dkname[i] );
	}


	for ( i = 0; i <= cdkname_no; i++ )  {
		fprintf( comdeck_list, "%s\n", cdkname[i] );
		free( cdkname[i] );
	}


#ifdef SAVE_LOG
	fprintf( logfile, "\n%d comdecks processed", cdkname_no  );
	fprintf( logfile, "\n%d decks processed\n", dkname_no  );
#endif

	fclose( input_file_handle );
	fclose( file_handle );
	fclose( deck_list );
	fclose( comdeck_list );


	puts( "Leaving create_library" );
   return(0);
}







/*
   Author: Ramesh Krishna  (Met. Office - Central Computing).

   Last Modified: 25 Dec 1992.

   Routine make_directory is used to create a subdirectory of the current.
   Currently it checks for files with the same name (regular files or
   directory files with the same name (regular files or directories )
   if they exist an error message is printed and execution is aborted.
*/


int make_directory( library_name, mode )
char *library_name; char *mode;
{
   char buffer[maxlen];
/*
   struct stat file_type;
   stat( library_name, &file_type );
*/


/* *** For creation first check if a file with the same name exists *** */

   if ( strncmp( mode, "create", 6 ) == 0 )  {

      if ( access( library_name, 00 ) == 0 )  {
         printf( "\nPlease delete file or directory \"%s\"", library_name );
         printf( "\nThen rerun the program\n" );
         return( user_error );
      }


      sprintf( buffer, "mkdir %s", library_name );  system( buffer );


      if ( access( library_name, 06 ) != 0 )  {
         printf( "Unable to use named directory \"%s\"", library_name );
         printf( "\nCheck umask and parent directory permissions\n" );
         return( user_error );
      }

   }
   else
   {
      printf( "\nCurrently, only creation of libraries is allowed\n" );
      return( user_error );
   }


   return(0);
}








/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

	Given an input file with update instructions (ie a PL) create an index.
	For each line of the input file the index file contains the line ID 
	followed by the type of line, i.e. (D)ECK  or  (C)OMDECK  and the 
	name of the (COM)DECK.

	input_file_name  -  name of the file containing the input data (a PL)
	pl_name          -  directory in which to save the x-ref file (with
	                    name index).

   Last Modified: 20 Apr 1993.
*/



int create_idx( input_file_name, pl_name )
char *input_file_name, *pl_name;
{
   char buffer[maxlen];
   char pl_index_name[max_name_len];
   FILE *tempfile;
   char *tempname;
   int  retcode;
   char ident[maxlen];
   char temp[maxlen];

   char type[max_name_len];
   char name[max_name_len];


   strcpy( type, "NONE" );
   strcpy( name, "NONE" );

/*  Open the program library and the index file to be created  */

   nupopen( pl, input_file_name, "r", user_error );

   sprintf( pl_index_name, "%s/index", pl_name );
   nupopen( pl_index, pl_index_name, "w", user_error );

/*  open a temporary file to save the unsorted index data  */

   if ( ( tempname = get_tempfile( "PXXXXXX\0" ) ) == NULL )
      return( unknown_error );

   nupopen( tempfile, tempname, "w", system_error );


/*  for each pl line read the index and write out to the temp file  */

   while ( fgets( buffer, maxlen, pl ) != NULL )  {

      if ( strncmp( buffer, "*DECK", 5 ) == 0 )  {
         strcpy( type, "D" );
         sscanf( buffer, "%*s%s", name );
      }

      if ( strncmp( buffer, "*COMDECK", 8 ) == 0 )  {
         strcpy( type, "C" );
         sscanf( buffer, "%*s%s", name );
      }

      get_ident( buffer, PL_id_name_start, temp );
      sscanf( temp, "%s", ident );   /* remove leading spaces */
      fprintf( tempfile, "%s %s %s\n", ident, type, name );
   }


   fclose( tempfile );
   fclose( pl );
	fclose( pl_index );



/*
   call the system function sort to sort the tempfile lines and save
   output into the index file
*/
/*
   sprintf( buffer, "cat %s | sort > %s", tempname, pl_index_name );
*/
   sprintf( buffer, "sort %s > %s", tempname, pl_index_name );
   system( buffer );
   unlink( tempname );


   return(0);
}







/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

	Given an input file with update instructions (ie a PL) create a cross
	reference file.
	For each COMDECK in the input file the x-ref file contains the 
	COMDECK name followed by names of all the DECKs that call the COMDECK.

	pl_name    -  name of the file containing the input data (a PL)
	xref_name  -  directory in which to save the x-ref file (with
	                    name xref).

   Last Modified: 22 May 1993.
*/




int create_xref( pl_name, xref_name )
char *pl_name, *xref_name;
{

	FILE *pl;


	char  temp[maxlen];
	char  buffer[maxlen];
	int   n, m;
	int   member;
	int   i, j;
	char  deckname[max_name_len];
	int   no_of_dependents;
	enum  t_boolean deckname_set = false;
	enum  t_boolean finish = false;
	char  filelist[max_decks][max_name_len];
	int   num_dependents;
	int   nest_level;
	int   retcode;
	char  decktype[2];

	char  *tempname;
	FILE  *tempfile;

	enum  t_boolean deck_already_saved;



	nupopen( pl, pl_name, "r", user_error );


/* Main loop.  Sweep through the PL extracting deck and comdeck names. */

	n = -1;

	while( fgets( buffer, maxlen-1, pl ) != NULL )  {

		if ( strncmp( buffer, "*DECK", 5 ) == 0  ||
		     strncmp( buffer, "*COMDECK", 8 ) == 0 )  {

			if ( sscanf( buffer, "%*s%s", temp ) == 0 )
				printf( "\nError in line:\n%s\n", buffer );
			else  {
				n++;
				Malloc_and_copy( cdeck[n].name, temp );
				*cdeck[n].type = buffer[1];
				cdeck[n].no_of_dependents = -1;
			}
		}
	}

	cdeck[n+1].name = NULL;

	n = -1;





/*
	Now sweep thro' the *CALL names and copy the corresponding *(COM)DECK
	name into the dependent field of the *CALL name entry structure.
*/

	rewind(pl);
	n = -1;

	while( fgets( buffer, maxlen-1, pl ) != NULL )  {

		if ( strncmp( buffer, "*DECK", 5 ) == 0  ||
		     strncmp( buffer, "*COMDECK", 8 ) == 0 )  {
			sscanf( buffer, "%*s%s", deckname );
			*decktype = buffer[1];
#ifdef DEBUG
printf( "\nDoing deck %s", deckname );
#endif
		}
	

		if ( strncmp( buffer, "*CALL", 5 ) == 0 )  {

			if ( sscanf( buffer, "%*s%s", temp ) == 0 )
				printf( "\nError in line:\n%s\n", buffer );
			else  {
				n = -1;
				while ( cdeck[++n].name != NULL )  {
					if ( strcmp( cdeck[n].name, temp ) == 0 )  {

/* #ifdef DEBUG
printf( "\ncomdeck_name= %s", cdeck[n].name );
#endif */

        /* Check that the dependent name hasn't already been saved */

					deck_already_saved = false;

					for ( j = 0; j <= cdeck[n].no_of_dependents; j++ )
						if ( strcmp( cdeck[n].dependent[j], deckname ) == 0 )
							deck_already_saved = true;

						if ( deck_already_saved == false )  {

							i = ++cdeck[n].no_of_dependents;
#ifdef DEBUG
printf( "\nCopying dependents: deck_no, dep_no, comdeck = %d, %d, %s", 
        n, i, cdeck[n].name );
fflush(stdout);
#endif
							Malloc_and_copy( cdeck[n].dependent[i], deckname );
							*cdeck[n].dependent_type[i] = *decktype; 
							break;
						}
					}
				}
			}
		}
	}


/*
	n = -1;
	while ( cdeck[++n].name != NULL )  {

		printf( "\nn, name, type = %d, %s, %s", n,cdeck[n].name,cdeck[n].type);

		for ( i = 0; i <= cdeck[n].no_of_dependents; i++ )  {
			printf( "\ni, dependent: %d %s", i, cdeck[n].dependent[i] );
		}
	}
*/


/* 
	For each dependent check if it has other dependents. If yes then add
	those to the current dependent list.
*/


   if ( ( tempname = get_tempfile( "PXXXXXX\0" ) ) == NULL )
      return( unknown_error );

#ifdef DEBUG
	printf( "\ntemp name = %s", tempname );
#endif

	nupopen( tempfile, tempname, "w", user_error );



	member = -1;
	nest_level = 0;

	while ( cdeck[++member].name != NULL )  {

#ifdef SAVE_LOG
		fprintf( logfile, "\n%s", cdeck[member].name );
#endif

		fprintf( tempfile , "\n%s", cdeck[member].name );
		num_dependents = -1;
		if ( ( retcode = get_dependencies( member, filelist, &num_dependents, 
		       &nest_level ) ) != 0 ) {
			fprintf( stderr, "\nError tracing dependencies for (COM)DECK\n%s\n",
			         cdeck[member].name );
			return( user_error );
		}
		if ( num_dependents >= 0 )
			for ( j = 0; j <= num_dependents; j++ )  {
				fprintf( tempfile, " %s", filelist[j] );
		}
	}



	fclose( pl );
	fputs( "\n", tempfile );
	fclose( tempfile );

   sprintf( buffer, "sort %s > %s/xref", tempname, xref_name );
   system( buffer );

	unlink( tempname );

	puts("\n" );
	return(0);
}






get_dependencies( member, filelist, num_dependents, nest_level )
int member; 
char filelist[max_decks][max_name_len]; 
int  *num_dependents; 
int  *nest_level;
{
	int n;
	int i, j;
	enum t_boolean flag;
	char tempname[max_name_len];

	*nest_level = *nest_level + 1;

	if ( *nest_level > max_call_nest )  {
		printf( "\nnesting level too large for (COM)DECK member %s", 
		        cdeck[member].name );
		printf( "\nCurrent calls:\n%s", filelist );
		*nest_level = *nest_level - 1;
		return( user_error );
	}

	for ( i = 0; i <= cdeck[member].no_of_dependents; i++ )  {
		if ( *cdeck[member].dependent_type[i] == 'D' )  {
			*num_dependents = *num_dependents + 1;
			strcpy( filelist[*num_dependents], cdeck[member].dependent[i] );
		}
	}


	for ( i = 0; i <= cdeck[member].no_of_dependents; i++ )  {

		n = -1;
		while( cdeck[++n].name != NULL )  {
			if ( strcmp( cdeck[n].name, cdeck[member].dependent[i] ) == 0 )  {
				if ( *cdeck[n].type == 'C' )
					get_dependencies( n, filelist, num_dependents, nest_level );
			}
		}
	}

	*nest_level = *nest_level - 1;
	return(0);
}


