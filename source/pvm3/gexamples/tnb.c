
static char rcsid[] =
	"$Id: tnb.c,v 1.2 1997/07/09 13:28:05 pvmsrc Exp $";

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
	test bcast node 
*/

#include <stdio.h>
#include "pvm3.h"

int
main(argc, argv)
int argc;
char *argv[];
{
	int mytid;
	char g[32];
	int cc, ngroup;
	int gid;

	mytid = pvm_mytid();
	pvm_recv(pvm_parent(), 5);
	pvm_upkstr(g);
	pvm_upkint(&ngroup, 1, 1);
	gid = pvm_joingroup(g);
	pvm_barrier(g, ngroup);
	pvm_recv(pvm_parent(), 77);
	pvm_lvgroup(g);
	pvm_exit();
}
