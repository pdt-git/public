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

package org.cs3.pl.prolog;

import java.io.PrintStream;

/**
 * @deprecated ld: i don't like this interface any more. It voices promises we
 *                   can hardly keep (at least for now). We WILL have a meens to
 *                   consult into a stream, but we will NOT have the other
 *                   functionality in suggested by this interface. In particular, the
 *                   record/reload mechanism turned out to be not as performant as
 *                   expected.
 * 
 *  
 */
public interface ConsultService {

    /**
     * provides a stream to which clauses can be written. The supplied data will
     * be consulted as "virtual" file with the symbolic name specified by
     * setSymbol as soon as the stream is closed or eof is written.
     * 
     * @deprecated avoid it if you can. this method will be moved soon.
     * @return an output stream to which clauses can be written.
     */
    public PrintStream getOutputStream(String symbol);

    /**
     * @deprecated cwe simply do not know!
     * @return true if and only if data has been consulted using the given
     *            symbolic filename
     */
    //public boolean isConsulted(String s);

    /**
     * @deprecated cannot give an adequat implementation. marks the given symbol
     *                   as deleted, effectively removing the facts previously bound
     *                   to it.
     *                   <p>
     *                   Note: i do not know of any "clean" way to implement this. The
     *                   contract for this method is. that it will try to emulate the
     *                   effect, e.g. by consulting an emty stream and marking the
     *                   symbol in an apropiate way.
     *                   <p>
     *                   If <code>s</code> is not a symbolic filename consulted with
     *                   <b>this </b> ConsultService, the call has no effect.
     */
    //public void unconsult(String s);

    /**
     * @deprecated cannot give an adequat implementation. get the symbol's time
     *                   stamp
     * 
     * @param s
     *                a symbolic file name.
     * @return the number of milliseconds between 00:00:00 GMT, January 1, 1970
     *            and instance the last modification was made to the data consulted
     *            as symbolic file s, or -1 If the information is not available for
     *            what reasons ever.
     * 
     *  
     */
    //public long getRecordTimeStamp(String s);

    /**
     * @param provider
     * @deprecated use
     *                   IPrologInterface.addPrologInterfaceListener(IPrologInterface.SUBJECT_CONSULT, _)
     */
    public void addConsultServiceListener(ConsultServiceListener provider);

    /**
     * @param provider
     * @deprecated use
     *                   IPrologInterface.addPrologInterfaceListener(IPrologInterface.SUBJECT_CONSULT, _)
     */
    public void removeConsultServiceListener(ConsultServiceListener provider);

    /**
     * reload recorded data. If the ConsultService supports recording of
     * consulted streams, it will try to reload any existing records. Otherwise
     * (or if no records are available) this method does nothing.
     * 
     * @throws IOException
     */
    // public void reload() throws IOException;
    /**
     * clear recorded data. if the ConsultService supports recording of
     * consulted streams, this will clear all existing records it currently
     * keeps.
     */
    //public void clearRecords();

    /**
     * @return true if and only if this consult service supports recording and
     *            recording is currently enabled.
     */
    //public boolean isRecording();

    /**
     * if this consult service supports recording, this method can be used to
     * switch it on or off.
     */
    //public void setRecording(boolean val);

    /**
     * 
     * @return true if the consult service appends consulted data to a already
     *            exisiting record.
     */
   // public boolean isAppendingRecords();

    /**
     * if this consult service supports appending to existing records, this
     * method can switch it on and off.
     */
    //public void setAppendingRecords(boolean val);
}
