/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

#ifndef CUSTOM_IO_H
#define CUSTOM_IO_H



/**
 * The Custom IO API can be used to implement io streams with custom behaviour 
 * directly in prolog. This is done by defining a small set of foreign predicates and hook
 * predicates that are documented below. 
 */



#define CIO_MAGIC 1318236

/* custom stream handle */

typedef struct streamhandle{
	int magic; /* an idea that does not work... :-( */
	IOSTREAM * stream; /* the stream this handle belongs to */
	record_t stream_record; /* record key of the handle term associated with the stream */
	record_t args_record; /* record key of the UserData term associated with the stream */
	module_t module; /* the module which defines the hook predicates */
	predicate_t read_hook; /* cio_read(+Stream,+UserData, +BufferSize, +TTYMode, -Chars) */
	predicate_t write_hook; /* cio_write(+Stream,+UserData,+Chars,-Tail) */
	predicate_t seek_hook; /*TODO*/
	predicate_t close_hook; /* cio_close(+Stream,+UserData) */
	predicate_t control_hook; /* currently not used, but reserved */
} HANDLE_T;

/* io callbacks, delgating work to the hook predicates.*/
static int delegate_read(void *_handle, char *buf, int bufsize);
static int delegate_write(void *_handle, char*buf, int bufsize);
static long delegate_seek(void *_handle, long offset, int whence);
static int delegate_close(void *_handle);
//static int delegate_control(void *_handle, int action, void *arg);




/* foreign predicates */


/**

cio_create(+Module, +UserData, +Mode, -Stream)

creates a new custom io stream.

Module should be the name of a module that implements the following hook predicates
cio_read/5, 
cio_write/4,
cio_close/2,
cio_seek/4 (TODO).

See below for information on how to implement these.

UserData can be any term. The same term will be given as an 
argument to each of the io hooks above. You can use this to 
associate some data with your stream.

Mode must be one of the atoms 'write' (if you want to create an ouput stream), or 'read'
if you want to implement an input stream.

Stream will be unified with a handle for the created stream.



Implementing the custom io hooks
--------------------------------

cio_read(+Stream,+UserData, +BufferSize, +TTYMode, -Chars)

This hook is called to fill the buffer of an input stream. Implementations
should unify Chars with a list of character atoms. If no input is available, 
the hook should block until some input is available. If the stream is at eof, Chars 
should be unified with nil, i.e. the empty list []. (TODO)
If the hook fails, or is not defined, an io exception will be raised. (TODO)

cio_write(+Stream,+UserData,+Chars,-Tail)

This hook is called when the buffer of an output stream is flushed. The data to be written is
passed as a list of character atoms in the Chars Argument. Implementations 
should process any portion of the data and unify the remainder with Tail.
The hook should block until at least some data has been processed. If the hook is not 
defined or fails, an io exception will be raised.(TODO)
NOTE: it is not advisable to unify the whole Chars list with Tail, i.e. perform a zero 
write. Although it is not an error to do this occasionaly, it will only result in the hook 
beeing called again with the same data. Implementations must pay attention not to drain
cpu time with idle loops. If no data can be processed, the hook should block until some data
can be processed, or fail to indicate that there is a permanent problem with the stream. 
(e.g. backend peer closed down, or something similar).

cio_close(+Stream,+UserData)

This hook is called when the stream is closed. Implementations should make sure to clean up
free any resource occupied by the implementation, etc. This is the last hook call back this
stream will ever recieve. 
If the hook fails, an io exception is raised(TODO). If it is undefined, the stream assumes
default behaviour (i.e. does nothing :-) )
 
 
cio_seek(....) TODO
*/
foreign_t pl_cio_create(term_t module_term, term_t args_term, term_t mode_term, term_t stream_term);


/**
 * 	cio_read_n_chars(+Stream,+Limit,-Chars).
 * 
 * Tries to read up to  Limit chars from Stream and unify a list of respective 
 * character atoms with Chars.
 * If Stream is at eof, Chars is unified with nil, i.e. the empty list []. (TODO)
 * A call to this predicate will block until some data is available, or eof is reached.
 */
foreign_t pl_cio_read_n_chars(term_t stream_term, term_t size_term, term_t chars_term);

#endif
