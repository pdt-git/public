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

/*
 */
package org.cs3.pl.prolog.internal.pifcom;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketImpl;
import java.net.UnknownHostException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.LogBuffer;
import org.cs3.pl.common.SimpleLogBuffer;
import org.cs3.pl.prolog.internal.Reusable;

/**
 */
public class ReusableSocket extends Socket implements Reusable {

    private LogBuffer logBuffer = new SimpleLogBuffer();

    /**
     * 
     */
    public ReusableSocket() {
        super();
        // TODO Auto-generated constructor stub
    }

    /**
     * @param host
     * @param port
     * @throws java.net.UnknownHostException
     * @throws java.io.IOException
     */
    public ReusableSocket(String host, int port) throws UnknownHostException,
            IOException {
        super(host, port);
    }

    

    /**
     * @param address
     * @param port
     * @throws java.io.IOException
     */
    public ReusableSocket(InetAddress address, int port) throws IOException {
        super(address, port);
    }

   

    /**
     * @param impl
     * @throws java.net.SocketException
     */
    public ReusableSocket(SocketImpl impl) throws SocketException {
        super(impl);
    }

    /**
     * @param host
     * @param port
     * @param localAddr
     * @param localPort
     * @throws java.io.IOException
     */
    public ReusableSocket(String host, int port, InetAddress localAddr,
            int localPort) throws IOException {
        super(host, port, localAddr, localPort);
    }

    /**
     * @param address
     * @param port
     * @param localAddr
     * @param localPort
     * @throws java.io.IOException
     */
    public ReusableSocket(InetAddress address, int port, InetAddress localAddr,
            int localPort) throws IOException {
        super(address, port, localAddr, localPort);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#reuse()
     */
    public void reuse() {
        logBuffer.log("socket","reuse");;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#destroy()
     */
    public void destroy() {
        logBuffer.log("socket","destroy");
        try {
            close();
        } catch (IOException e) {
	        Debug.report(e);
	        throw new RuntimeException(e.getMessage());
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#recylce()
     */
    public void recylce() {
        logBuffer.log("socket","recycle");
    }

    /**
     * @return
     */
    public LogBuffer getLogBuffer() {
        return this.logBuffer;
    }

}
