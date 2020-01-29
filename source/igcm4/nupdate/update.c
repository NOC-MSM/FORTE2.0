#include "defns.h"
#include "structs.h"

#ifndef CRAY
#include <sys/fcntl.h>
#endif



argument_def   argument;
directive_def  directive;
info_def       info;


enum t_boolean   seq_nos;
char           master[2];
FILE           *logfile;

char           *rbsearch2();

/*  #define logfile_name "update.log" */   /* 15-11-95  Marc */

int   tmpfile_num = 0;  /* used for creating different names for IBD files */

int   get_outfile_name_2();

atexit_t  cleanup();

char tmpdir[15];     /* Added 14-11-95  Marc Chippindale */
char PL_tmp[13];     /* Added 15-11-95  Marc Chippindale */
char index_tmp[19];  /* Added 15-11-95  Marc Chippindale */
char xref_tmp[18];  /* Added 15-11-95  Marc Chippindale */
char dirv_tmp[22];   /* Added 15-11-95  Marc Chippindale */





main( argc, argv )
int   argc;
char  **argv;
{

	int   arg_no;
	int   n;
        int   pid;     /* Added 15-11-95  Marc Chippindale */
	int   i;
	char  *args[max_decks];
	int   numargs;
	int   retcode;
	char  buffer[maxlen];
	FILE  *fp;
	char logfile_name[15];  /* Added 15-11-95  Marc Chippindale */

	pid = getpid();
sprintf( tmpdir, "_up%d.tmp\0", pid );  /* Added 14-11-95  Marc Chippindale */
sprintf( PL_tmp, "_up%d.tmp\0", pid );  /* Added 15-11-95  Marc Chippindale */
sprintf( index_tmp, "_up%d.tmp/index\0", pid );  /* Added 15-11-95  Marc Chippindale */
sprintf( xref_tmp, "_up%d.tmp/xref\0", pid );  /* Added 15-11-95  Marc Chippindale */
sprintf( dirv_tmp, "_up%d.tmp/dirv.tmp\0", pid );  /* Added 15-11-95  Marc Chippindale */
sprintf( logfile_name, "update%d.log", pid );  /* Added 15-11-95  Marc Chippindale */

   if ( ( retcode = atexit( cleanup ) ) != 0 )  {
      printf( "\nError calling atexit. retcode = %d", retcode );
		fflush( stdout );
      exit(1);
   }


	directive.num_of_defines  = -1;
	directive.num_of_compiles = -1;


	argument.directives_file    = NULL;
	argument.PL                 = NULL;
	argument.compile_file       = NULL;
	argument.source_file        = NULL;
	argument.verbose            = NULL;

	argument.num_of_options = -1;

	*argument.mode = 'n';   /* normal update by default */
	argument.suffix = NULL;
	argument.split_output = 0;  /* Added 9/2/96  Paul Burton */

	arg_no = 0;

#ifdef SAVE_LOG
	nupopen( logfile, logfile_name, "w", system_error );
#endif

	print_banner();

	if ( argc == 1 )  {
		printf("\nUsage: update -i directives_file -p library " );
		printf("-c compile_file -a suffix -d defs -o options -q decks -D\n" );
		exit( user_error );
	}


/*  temporary directory for temporary files  */

	makedir( tmpdir );


	for (;;)  {

		arg_no++;

		if ( arg_no >= argc )  break;


		if ( strcmp( argv[arg_no], "-i" ) == 0 )  {

			if ( arg_no == argc - 1 )  
				prt_error( "Missing option for argument -i", "Aborting", 
				            user_error );

			argument.directives_file = argv[++arg_no];

			if ( access( argument.directives_file, 04 ) != 0 )
				prt_error( "Unable to read file:", argument.directives_file,
				           user_error );
		}


		else if ( strcmp( argv[arg_no], "-p" ) == 0 )  {

			if ( arg_no == argc - 1 )  
				prt_error( "Missing option for argument -p", "Aborting", 
				            user_error );

			argument.PL = argv[++arg_no];

			if ( access( argument.PL, 04 ) != 0 )
				prt_error( "Unable to read file:", argument.PL, user_error );

		}


		else if ( strcmp( argv[arg_no], "-c" ) == 0 )  {

			if ( arg_no == argc - 1 )  
				prt_error( "Missing option for argument -c", "Aborting", 
				            user_error );

			n = strlen( argv[++arg_no] ) + 3;
			Malloc( argument.compile_file, n, system_error );
			strcpy( argument.compile_file, argv[arg_no] );

		}

/* -D argument added by Paul Burton 9/2/96  */		
		else if ( strcmp( argv[arg_no], "-D" ) ==0 )  {
		  
		  argument.split_output=1;
		  
		}

		else if ( strcmp( argv[arg_no], "-s" ) == 0 )  {

			if ( arg_no == argc - 1 )  
				prt_error( "Missing option for argument -s", "Aborting", 
				            user_error );

			argument.source_file = argv[++arg_no];

		}	


		else if ( strcmp( argv[arg_no], "-q" ) == 0 )  {   /* copy args dirv */

			/*  copy options to directive.define (separated by commas) */

			*argument.mode = 'q';

			if ( arg_no < argc - 1  &&  *argv[arg_no+1] != '-' )  {

				numargs = process_list( argv[++arg_no], args, max_decks-1 );

				for ( i = 0; i <= numargs; i++ )  {

					if ( *args[i] == '-' )  break;

					directive.num_of_compiles++;

					Malloc_and_copy( directive.compile[i], args[i] );

				}

				for ( i = 0; i <= numargs; i++ )  free(args[i]);

			}

		}


		else if ( strcmp( argv[arg_no], "-a" ) == 0 )  {   /* copy args dirv */

			if ( arg_no == argc - 1 )
				prt_error( "Missing option for argument -a", "Aborting", 
				            user_error );

			argument.suffix = argv[++arg_no];

		}


		else if ( strcmp( argv[arg_no], "-m" ) == 0 )  {   /* copy args dirv */

			if ( arg_no == argc - 1 )

				argument.verbose = argv[arg_no];

			else  {

				if ( *argv[arg_no+1] != '-' )
					argument.verbose = argv[++arg_no];
				else
					argument.verbose = argv[arg_no];
			}

		}


		else if ( strcmp( argv[arg_no], "-f" ) == 0 )  { 

			*argument.mode = 'f';

		}


		else if ( strcmp( argv[arg_no], "-d" ) == 0 )  {   /* copy args dirv */

			/*  copy options to directive.define (separated by spaces) */

			if ( arg_no == argc - 1 )
				prt_error( "No options for argument -d", " ", user_error );

			if ( *argv[arg_no+1] == '-' )
				prt_error( "No options for argument -d", " ", user_error );


			n = arg_no;

			if ( strstr( argv[n+1], "," ) != NULL )  {

				numargs = process_list( argv[++n], args, max_defs - 1 );

				for ( i = 0; i <= numargs; i++ )  {

					directive.num_of_defines++;

					Malloc_and_copy( directive.define[i], args[i] );

				}

				for ( i = 0; i <= numargs; i++ ) free(args[i]);
				n++;

			}
			else  {

				while( ++n <= argc-1  &&  *argv[n] != '-' )  {

					i = ++directive.num_of_defines;

					Malloc_and_copy( directive.define[i], argv[n] );

				}
			}


			if ( n == argc )  

				arg_no = argc;  /* exit next loop */

			else

				arg_no = n-1;

		}


		else if ( strcmp( argv[arg_no], "-o" ) == 0 )  {

			/*  copy options to argument.option */

			if ( arg_no == argc - 1 )
				prt_error( "No options for argument -o", " ", user_error );

			n = arg_no;

			while( ++n <= argc - 1  &&  *argv[n] != '-' )  {

				i = ++argument.num_of_options;

				Malloc_and_copy( argument.option[i], argv[n] );

			}


			if ( n == argc )  

				arg_no = argc;  /* exit next loop */

			else

				arg_no = n-1;


			rbsort( argument.option, argument.num_of_options );

		}

		else

			prt_error( "Unrecognised option:", argv[arg_no], user_error );

	}


/*
	Check arguments
	----------------------------------------------------------------------
*/


/*
	argument.suffix[1] = '\0';
	strcat( argument.compile_file, ".\0" );
	strcat( argument.compile_file, argument.suffix );
*/



	if ( argument.directives_file != NULL ) 
		printf( "\ndirectives_file = %s", argument.directives_file );
	else {
		printf( "\ndirectives_file not entered, using %s", dirv_tmp );
		mkfile( dirv_tmp, "*ID RAMESH\n\0" );
		Malloc_and_copy( argument.directives_file, dirv_tmp );
	}


	if ( argument.PL != NULL )
		printf( "\nPL = %s", argument.PL );
	else {
		printf( "\nPL name not entered" );
		Malloc_and_copy( argument.PL, tmpdir );
		mkfile( index_tmp, "*/\n\0" );
		mkfile( xref_tmp, "*/\n\0" );
	}

/* Tests added by Paul Burton 9/2/96  for -D option */   
     
    if ( argument.compile_file != NULL ) {
    	if (argument.split_output==1) {
    		prt_error("\nError in update control statement: -D and -c\n",
		  	     	  "\nAborting\n" , user_error);
		}
		else {
			if ( ( retcode = process_suffix( argument.compile_file, argument.suffix )
			) != 0 )  return( retcode ); 
			printf( "\ncompile_file = %s", argument.compile_file );
		}
	}
	
	if ( argument.compile_file == NULL && argument.split_output==0) {
          prt_error("\nError in update control statement: -D or -c must be specified\n",
                    "\nAborting\n", user_error);
    }

	if ( argument.verbose != NULL )  {

		if ( argument.source_file != NULL )
			printf( "\nsource_file = %s", argument.source_file );
		else
			puts( "\nsource file not entered" );
	
	
		if ( argument.suffix != NULL )
			printf( "\nsuffix = %s", argument.suffix );
		else
			puts( "\nsuffix not enetered" );
	
	
		if ( argument.num_of_options >= 0 )  {
			for ( i = 0; i <= argument.num_of_options; i++  )
				printf( "\ni, option %d = %s", i, argument.option[i] );
		}
		else
			puts( "\nNo -o options" );
	
	
		if ( directive.num_of_compiles >= 0 )  {
	
			for ( i = 0; i <= directive.num_of_compiles; i++ )
	
				printf( "\ni,compile = %d, %s", i, directive.compile[i] );
		}
		else
			puts( "\nno -q options" );
	
	
		if ( directive.num_of_defines >= 0 )  {
	
			for ( i = 0; i <= directive.num_of_defines; i++ )
	
				printf( "\ni,define = %d, %s", i, directive.define[i] );
		}
		else
			puts( "\nno -d options" );
     	}




#ifdef DEBUG
	printf( "\nmode = %c", *argument.mode );
#endif

fflush( stdout );

/* 
   from proc_dir main
   -------------------------------------------------------------------------
*/


   if ( rbsearch2( argument.option, argument.num_of_options, "sq\0" ) != NULL )
      seq_nos = true;
   else
      seq_nos = false;

 


	if ( ( retcode = proc_dir( argument.directives_file, &directive ) ) != 0 )
		exit( retcode );

	if ( ( retcode = get_deck_names( &directive, argument.PL ) ) != 0 )
		exit( retcode );

	if ( ( retcode = resolve_comdeck_calls( &directive, argument.PL ) ) != 0 )
		exit(retcode );


#ifdef SAVE_LOG
	fprintf( logfile, "\nMode = %c", *argument.mode );
	fprintf( logfile, "\n\nident = %s", directive.ident );

	fprintf( logfile, "\n\nno_compiles = %d", directive.num_of_compiles );
	for ( i = 0; i <= directive.num_of_compiles; i++ )  {
		fprintf( logfile, "\n%d = %s", i, directive.compile[i] );
	}


	fprintf( logfile, "\n\nno_defines = %d", directive.num_of_defines );
	for ( i = 0; i <= directive.num_of_defines; i++ )  {
		fprintf( logfile, "\n%d = %s", i, directive.define[i] );
	}


	fprintf( logfile, "\nnum_of_comdecks = %d", directive.num_of_comdecks );
   for ( i = 0; i <= directive.num_of_comdecks; i++ )  {
      fprintf( logfile, "\n%d = %s", i, directive.comdeck[i] );
   }

	fprintf( logfile, "\nnum_of_decks = %d", directive.num_of_decks );
   for ( i = 0; i <= directive.num_of_decks; i++ )  {
      fprintf( logfile, "\n%d = %s", i, directive.deck[i] );
   }





	fputs( "\n------------------------------------------", logfile );
	fprintf( logfile, "\n\nno_IBDs = %d", directive.num_of_IBDs );
	for ( i = 0; i <= directive.num_of_IBDs; i++ )  {
		fputs( "\n", logfile );
		fprintf( logfile, "\n%d) type = %s", i, directive.IBD[i].type );

		if ( directive.IBD[i].declare != NULL ) 
			fprintf( logfile, "\nident, declare = %s, %s", 
		        directive.IBD[i].ident, directive.IBD[i].declare );
		else
			fprintf( logfile, "\nident, declare = %s, (null)", 
		        directive.IBD[i].ident, directive.IBD[i].declare );

      if ( directive.IBD[i].file != NULL )
			fprintf( logfile, "\nfile = %s", directive.IBD[i].file );
		else
			fprintf( logfile, "\nfile = (null)", directive.IBD[i].file );


		if ( directive.IBD[i].line[0] != NULL )  {

			if ( directive.IBD[i].line[1] != NULL )
				fprintf( logfile, "\nline1,2 = %s, %s", directive.IBD[i].line[0],
	        		directive.IBD[i].line[1] );
			else
				fprintf( logfile, "\nline1,2 = %s, (null)", directive.IBD[i].line[0],
	        		directive.IBD[i].line[1] );
			}

			if ( directive.IBD[i].deck_type != NULL )
				fprintf( logfile, "\ndeck_type = %s", directive.IBD[i].deck_type );

         if ( directive.IBD[i].PL_file != NULL )
				fprintf( logfile, "\nPL_file = %s", directive.IBD[i].PL_file );
	}

#endif

/* We derive the master character from the PL unless (its stored in a file
   called master.  However, update can be used without specifying a
   PL.  In this case set the default, contained in Master_char.
*/

	n = strlen( PL_tmp );
	if ( strncmp( argument.PL, PL_tmp, n ) == 0 )  {
		*directive.master = Master_char;
		*master           = Master_char;
	}
	else  {

	*directive.master = read_master( argument.PL );
	*master           = read_master( argument.PL );

	}
/*
	if ( strcmp( argument.PL, tmpdir ) == 0 )  {

		*directive.master = Master_char2;
		*master           = Master_char2;

	}
*/
	proc_pl( &directive, argument.PL, &info, argument.compile_file );





#ifdef SAVE_LOG

	fflush( logfile );

/* print out directives which were not executed  */
	n = 0;

	for ( i = 0; i <= directive.num_of_IBDs; i++ )  {

		if ( directive.IBD[i].executed == false )  {
			if ( strcmp( directive.IBD[i].type, "INSERT" ) == 0  ||
			     strcmp( directive.IBD[i].type, "BEFORE" ) == 0  ||
              strcmp( directive.IBD[i].type, "DELETE" ) == 0  )  {

				if ( n == 0 )  {
					fprintf( stderr, "\nThe following directives were ignored" );
					fprintf( stderr, "\n-------------------------------------" );
					fprintf( logfile, "\n\nThe following directives were ignored" );
					fprintf( logfile, "\n-------------------------------------" );
					n = 1;
				}

				if ( directive.IBD[i].line[1] != NULL )  {
					fprintf( stderr, "\n*%s %s,%s", directive.IBD[i].type,
						directive.IBD[i].line[0], directive.IBD[i].line[1] );
					fprintf( logfile, "\n*%s %s,%s", directive.IBD[i].type,
						directive.IBD[i].line[0], directive.IBD[i].line[1] );
				}
				else  {
					fprintf( stderr, "\n*%s %s", directive.IBD[i].type,
						directive.IBD[i].line[0] );
					fprintf( logfile, "\n*%s %s", directive.IBD[i].type,
						directive.IBD[i].line[0] );
				}

			}
		}
	}

	fputs( "\n\nFinished update in main\n\n", logfile );
	fflush(stdout);
	fclose(logfile);

#endif


/* delete temporary files */

/*  
    *** work done below is now done by cleanup

	for ( i = 0; i <= directive.num_of_IBDs; i++ )  {
		unlink( directive.IBD[i].file );
	}

	for ( i = 0; i <= directive.num_of_decks; i++ )  {
		get_outfile_name_2( directive.deck[i], "D", buffer );
		unlink( buffer );
	}

	for ( i = 0; i <= directive.num_of_comdecks; i++ )  {
		get_outfile_name_2( directive.comdeck[i], "C", buffer );
		unlink( buffer );
	}


	unlink( index_tmp );
	unlink( xref_tmp );


	if ( delete_dir( tmpdir ) != 0 )  return( retcode );
*/


	puts( "\n\nFinished update in main\n\n" );
	return(0);
}




print_banner()
{
	fprintf( stdout, "\nPortable update utility" );
	fprintf( stdout, "\nBeta release v%s\n", RELEASE );

#ifdef SAVE_LOG
	fprintf( logfile, "\nPortable update utility" );
	fprintf( logfile, "\nBeta release v%s\n", RELEASE );
#endif

}
