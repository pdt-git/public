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

package org.cs3.pl.console;

/**
 * Abstract model of a console.
 * 
 * A Console, as modeled by this interface, mainly consists of a buffer
 * containing a line of text that can be arbitrarily modified and finaly
 * comitted. In addition, a console produces output, which can be recieved by
 * registering an apropiate listener with this console. Note that the model by
 * itself does not support any buffering of the produced output.
 * 
 * As an (optional) feature, a console may support two different kinds of input
 * processing: the "normal" mode is described above, the alternative is the
 * so-called "single char" mode. When in single char mode, the underlying
 * streams expect character-wise unbuffered input via the putSingleChar(char)
 * method.
 */
public interface ConsoleModel {

	/**	 
	 * @return the current content of the linebuffer.
	 */
	abstract public String getLineBuffer();

	/**
	 * set the content of the line buffer. 
	 * @param line
	 */
	abstract public void setLineBuffer(String line);

	/**
	 * commit the current content of the linebuffer (i.e. "press enter").	 
	 */
	abstract public void commitLineBuffer();

	/**
	 * Register a ConsoleModelListerner with this console.
	 * @param cml
	 */
	abstract public void addConsoleListener(ConsoleModelListener cml);

	/**
	 * remove a registered ConsoleModelLister from the list of registered Listeners.
	 * @param cml
	 */
	abstract public void removeConsoleListener(ConsoleModelListener cml);

	/**
	 * Send a single char to the underlying stream.
	 * Wether the input buffer is modified or not, is up to the respective implementation.
	 * <p><P>It is concidered a bad practice to call this method while the model is not in single
	 * char mode, although some implementations might allow it, this should generaly throw some kind
	 * of runtime exception.  
	 * @param c
	 */
	abstract public void putSingleChar(char c);

	/**
	 * @return true if and only if the console is currently in single char mode.
	 */
	abstract public boolean isSingleCharMode();

	/**
	 * tell the model to connect to the underlying streams.
	 */
	abstract public void connect();
	
	/**
	 * tell the model to disconnect from the underlying streams.
	 */
	abstract public void disconnect();
	
	/**
	 * @return true if and only if the console is connected to the underlying
	 * streams, i.e. it is fully operational.
	 */
	abstract public boolean isConnected();

}