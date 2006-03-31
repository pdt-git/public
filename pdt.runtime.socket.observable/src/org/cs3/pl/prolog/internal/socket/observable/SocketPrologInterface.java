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

package org.cs3.pl.prolog.internal.socket.observable;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.cs3.pl.prolog.internal.ReusablePool;

public class SocketPrologInterface extends AbstractPrologInterface {

    private class InitSession extends SocketSession {
        public InitSession(SocketClient client, PrologInterface pif)
                throws IOException {
            super(client, pif);
        }

        public void dispose() {
            Debug.error("Trying to dispose the initial session!");
        }

        public void doDispose() {
            super.dispose();
        }
    }

    private class ShutdownSession extends SocketSession {
        public ShutdownSession(SocketClient client, PrologInterface pif)
                throws IOException {
            super(client, pif);
        }

        public void dispose() {
            Debug.error("Trying to dispose the shutdown session!");
        }

        public void doDispose() {
            super.dispose();
        }
    }

    private int port = 9966;

    private boolean standAloneServer = false;

    private boolean useSessionPooling = true;

    private ReusablePool pool = useSessionPooling ? new ReusablePool() : null;

    public final static String EXECUTABLE = "pif.executable";

    public final static String ENGINE_DIR = "pif.engine_dir";

    public final static String PORT = "pif.port";

    public final static String STANDALONE = "pif.standalone";

    public static final String ENGINE_FILE = "pif.engine_file";

    public static final String MAIN_FILE = "pif.main_file";

    public final static String USE_POOL = "pif.use_pool";

    private String engineDir;

    private String executable;

    private PrologInterfaceFactory factory;

    private ResourceFileLocator locator;

    private HashMap consultServices = new HashMap();

    private PrologEventDispatcher prologEventDispatcher;

    public SocketPrologInterface(PrologInterfaceFactory factory) {
        super();
        this.factory = factory;

    }

    public PrologSession getSession_impl() throws Throwable {

        try {
            SocketClient client = getClient();
            SocketSession s = new SocketSession(client, this);
            return s;
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    private SocketClient getClient() {
        ReusableSocket socket = null;
        if (useSessionPooling) {
            socket = (ReusableSocket) pool.findInstance(ReusableSocket.class);
        }
        if (socket == null) {
            Debug.info("creating new ReusableSocket");
            try {
                socket = new ReusableSocket((String)null, port);
            } catch (Exception e) {
                Debug.report(e);
                throw new RuntimeException(e);
            }
        } else {
            Debug.info("reusing old ReusableSocket");
        }
        SocketClient client = null;
        try {
            client = new SocketClient(socket);
        } catch (IOException e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
        client.setPool(pool);
        return client;
    }

    public void setPort(int port) {
        if (isDown()) {
            this.port = port;
        } else {
            throw new IllegalStateException("Cannot change port while in use.");
        }
    }

    /**
     * @param standAloneServer
     *                The standAloneServer to set.
     */
    public void setStandAloneServer(boolean standAloneServer) {
        if (isDown()) {
            this.standAloneServer = standAloneServer;
        } else {
            throw new IllegalStateException("Cannot change port while in use.");
        }

    }

    /**
     * @param useSessionPooling
     *                The useSessionPooling to set.
     */
    public void setUseSessionPooling(boolean useSessionPooling) {
        this.useSessionPooling = useSessionPooling;
        pool = useSessionPooling ? new ReusablePool() : null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#setOption(java.lang.String,
     *         java.lang.String)
     */
    public void setOption(String opt, String value) {
        if (PORT.equals(opt)) {
            setPort(Integer.parseInt(value));
        } else if (ENGINE_DIR.equals(opt)) {
            this.engineDir = value;
        } else if (EXECUTABLE.equals(opt)) {
            this.executable = value;
        } else if (STANDALONE.equals(opt)) {
            setStandAloneServer(Boolean.valueOf(value).booleanValue());
        } else if (USE_POOL.equals(opt)) {
            setUseSessionPooling(Boolean.valueOf(value).booleanValue());
        } else {
            throw new IllegalArgumentException("option not supported: " + opt);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#getOption(java.lang.String)
     */
    public String getOption(String opt) {
        //ld: changed semantic:: System properties override any settings
        String s = System.getProperty(opt);
        if (s != null) {
            Debug.warning("option " + opt
                    + " is overridden by System Property: " + s);
            return s;
        }
        if (PORT.equals(opt)) {
            return "" + port;
        } else if (ENGINE_DIR.equals(opt)) {
            return engineDir;
        } else if (EXECUTABLE.equals(opt)) {
            return executable;
        } else if (STANDALONE.equals(opt)) {
            return "" + standAloneServer;
        } else if (USE_POOL.equals(opt)) {
            return "" + useSessionPooling;
        } else {
            throw new IllegalArgumentException("option not supported: " + opt);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#disposeInitialSession(org.cs3.pl.prolog.PrologSession)
     */
    protected void disposeInitialSession(PrologSession initSession) {
        InitSession s = (InitSession) initSession;
        s.doDispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#disposeShutdownSession(org.cs3.pl.prolog.PrologSession)
     */
    protected void disposeShutdownSession(PrologSession s) {
        ShutdownSession ss = (ShutdownSession) s;
        ss.doDispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#getInitialSession()
     */
    protected PrologSession getInitialSession() {
        try {
            return new InitSession(new SocketClient((String)null, port), this);
        } catch (Throwable e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#getShutdownSession()
     */
    protected PrologSession getShutdownSession() {
        try {
            return new ShutdownSession(new SocketClient((String)null, port),
                    this);
        } catch (Throwable e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#getConsultService(java.lang.String)
     */
    public ConsultService createConsultService(final String prefix) {
        final RecordingConsultService cs = new RecordingConsultService();
        cs.setRecording(false);
        cs.setPort(port);
        File file = getFactory().getResourceLocator().resolve(prefix);
        cs.setPrefix(file);
        addLifeCycleHook(new LifeCycleHook() {
            public void onInit(PrologInterface pif, PrologSession initSession) {
                cs.setPort(port);
                File file = new DefaultResourceFileLocator(new File(
                        getOption(ENGINE_DIR))).resolve(prefix);
                cs.setPrefix(file);

                try {
                    cs.connect();
                } catch (IOException e) {
                    Debug.report(e);
                    throw new RuntimeException(e);
                }
            }

            public void afterInit(PrologInterface pif) {
                ;
            }

            public void beforeShutdown(PrologInterface pif,
                    PrologSession session) {
                cs.disconnect();
            }
        }, "SocketPrologInterface.reloadCS." + prefix, new String[0]);
        //      this is NOT the same as isUp().
        // e.g. a hook may be added as a side effect of processing another
        // hook's.
        //onInit() method.
        //The newly added hook would not be processed for that init phase.
        // Still
        //the pif is NOT up yet, so we would miss one oportunity to reload!
        if (!isDown()) {
            try {

                cs.connect();

            } catch (IOException e) {
                Debug.report(e);
                throw new RuntimeException(e);
            }
        }

        return cs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#getFactory()
     */
    public PrologInterfaceFactory getFactory() {
        return factory;
    }

    /**
     * @return Returns the locator.
     */
    public ResourceFileLocator getLocator() {
        return locator;
    }

    /**
     * @param locator
     *                The locator to set.
     */
    public void setLocator(ResourceFileLocator locator) {
        this.locator = locator;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologInterface#getConsultService(java.lang.String)
     */
    public ConsultService getConsultService(String prefix) {
        ConsultService cs = (ConsultService) consultServices.get(prefix);
        if (cs == null) {
            cs = createConsultService(prefix);
            consultServices.put(prefix, cs);
        }
        return cs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#stop()
     */
    public synchronized void stop() {
        super.stop();
        pool.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#stop()
     */
    public synchronized void start() throws IOException {
        pool.clear();
        super.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#addPrologInterfaceListener(java.lang.String,
     *         org.cs3.pl.prolog.PrologInterfaceListener)
     */
    public void addPrologInterfaceListener(String subject,
            PrologInterfaceListener l) {
        PrologEventDispatcher d = getPrologEventDispatcher();
        d.addPrologInterfaceListener(subject, l);
    }

    /**
     * @return
     */
    private PrologEventDispatcher getPrologEventDispatcher() {
        if (prologEventDispatcher == null) {
            try {
                prologEventDispatcher = new PrologEventDispatcher(this);
                addLifeCycleHook(new LifeCycleHook() {

                    public void onInit(PrologInterface pif,
                            PrologSession initSession) {
                        ;
                    }

                    public void afterInit(PrologInterface pif) {
                        SocketClient client = getClient();
                        prologEventDispatcher.start(client);
                    }

                    public void beforeShutdown(PrologInterface pif,
                            PrologSession session) {
                      prologEventDispatcher.stop();
                    }
                }, "PrologEventDispatcher", new String[0]);
            } catch (IOException ioe) {
                Debug.report(ioe);
                throw new RuntimeException(ioe);
            }
            if(isUp()){
                SocketClient client = getClient();
                prologEventDispatcher.start(client);
            }
        }
        return prologEventDispatcher;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#removePrologInterfaceListener(java.lang.String,
     *         org.cs3.pl.prolog.PrologInterfaceListener)
     */
    public void removePrologInterfaceListener(String subject,
            PrologInterfaceListener l) {
        PrologEventDispatcher d = getPrologEventDispatcher();
        d.removePrologInterfaceListener(subject, l);

    }
}