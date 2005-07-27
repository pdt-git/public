package org.cs3.pl.prolog;

import java.util.EventObject;

/**
 * @deprecated
 */
public class ConsultServiceEvent extends EventObject {
    private static final long serialVersionUID = 1L;
    private String symbol;

    public ConsultServiceEvent(Object source) {
        super(source);
        this.symbol=null;        
    }
    public ConsultServiceEvent(Object source,String symbol) {
        super(source);
        this.symbol=symbol;        
    }
	/**
	 * @return the symbol for which the consulted data changed, or null after a bulk change.
	 */
    public String getSymbol(){
        return symbol;
    }
}
