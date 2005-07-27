/*
 */
package org.cs3.pl.prolog;

import java.util.EventObject;

/**
 */
public class PrologInterfaceEvent extends EventObject {
    /**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    private String subject;
    private String event;

    /**
     * @return Returns the event.
     */
    public String getEvent() {
        return event;
    }
    /**
     * @return Returns the subject.
     */
    public String getSubject() {
        return subject;
    }
    /**
     * @param source
     * @param event
     * @param subject
     */
    public PrologInterfaceEvent(Object source, String subject, String event) {
        super(source);
        this.subject=subject;
        this.event=event;
    }
    
}
