
static char rcsid[] =
	"$Id: pvmdabuf.c,v 1.5 1999/07/08 19:00:10 kohl Exp $";

/*
 *         PVM version 3.4:  Parallel Virtual Machine System
 *               University of Tennessee, Knoxville TN.
 *           Oak Ridge National Laboratory, Oak Ridge TN.
 *                   Emory University, Atlanta GA.
 *      Authors:  J. J. Dongarra, G. E. Fagg, M. Fischer
 *          G. A. Geist, J. A. Kohl, R. J. Manchek, P. Mucci,
 *         P. M. Papadopoulos, S. L. Scott, and V. S. Sunderam
 *                   (C) 1997 All Rights Reserved
 *
 *                              NOTICE
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted
 * provided that the above copyright notice appear in all copies and
 * that both the copyright notice and this permission notice appear in
 * supporting documentation.
 *
 * Neither the Institutions (Emory University, Oak Ridge National
 * Laboratory, and University of Tennessee) nor the Authors make any
 * representations about the suitability of this software for any
 * purpose.  This software is provided ``as is'' without express or
 * implied warranty.
 *
 * PVM version 3 was funded in part by the U.S. Department of Energy,
 * the National Science Foundation and the State of Tennessee.
 */

/*
 *	pvmdabuf.c
 *
 *	Data buffer manip.
 *
 * $Log: pvmdabuf.c,v $
 * Revision 1.5  1999/07/08 19:00:10  kohl
 * Fixed "Log" keyword placement.
 * 	- indent with " * " for new CVS.
 *
 * Revision 1.4  1997/06/25  22:09:20  pvmsrc
 * Markus adds his frigging name to the author list of
 * 	every file he ever looked at...
 *
 * Revision 1.3  1997/03/06  21:10:17  pvmsrc
 * 	quad align dabufs by adding double q[2] in alignme
 *
 * Revision 1.2  1997/01/28  19:27:04  pvmsrc
 * New Copyright Notice & Authors.
 *
 * Revision 1.1  1996/09/23  23:44:31  pvmsrc
 * Initial revision
 *
 * Revision 1.2  1994/06/03  20:38:23  manchek
 * version 3.3.0
 *
 * Revision 1.1  1993/08/30  23:26:50  manchek
 * Initial revision
 *
 */

#include <pvm3.h>
#include "pvmalloc.h"

union alignme {
	long l;
	char *p;
	double d;
	double q[2];		/* quad-word alignment */
};

#define	RCOFFSET	sizeof(union alignme)


/***************
 **  Private  **
 **           **
 ***************/


char *
da_new(len)
	int len;
{
	char *p;

	if (p = TALLOC(len + RCOFFSET, char, "data")) {
		p += RCOFFSET;
		*(int*)(p - sizeof(int)) = 1;
	}
	return p;
}


void
da_ref(p)
	char *p;
{
	++*(int*)(p - sizeof(int));
}


void
da_unref(p)
	char *p;
{
	if (--*(int*)(p - sizeof(int)) < 1)
		PVM_FREE(p - RCOFFSET);
}


