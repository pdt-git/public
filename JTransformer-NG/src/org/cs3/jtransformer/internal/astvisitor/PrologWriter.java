/*
 */
package org.cs3.jtransformer.internal.astvisitor;

import java.util.List;

/**
 */
public class PrologWriter implements IPrologWriter {
	
private List clauses;


//    private Writer out;

    /**
     * @param out
     * @param interpreted
     */
    public PrologWriter(List list, boolean interpreted) {
        super();
        this.clauses = list;
        this.interpreted = interpreted;
    }

    private boolean interpreted;


    private static final String INTE = "inTe";


	private static final String ASSERT = "assert";

	
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#setIndentionLevel(int)
     */
    public void setIndentionLevel(int i) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#addIndention()
     */
    public void addIndention() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#reduceIndention()
     */
    public void reduceIndention() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#writeFact(java.lang.String,
     *           java.lang.String[])
     */
    public void writeFact(String string, String[] param) {
        StringBuffer trm = term(string, param);
        String s = interpreted ? term(INTE, trm).toString() : 
        	term(ASSERT,trm.toString()).toString();
        
        clauses.add(s);
    }

    private StringBuffer term(String functor, String arg) {
        return term(functor, new String[] { arg });
    }

    private StringBuffer term(String functor, StringBuffer arg) {
        return term(functor, new StringBuffer[] { arg });
    }

    private StringBuffer term(String functor, String[] args) {
        StringBuffer sb = new StringBuffer();
        sb.append(functor);
        sb.append('(');
        for (int i = 0; i < args.length; i++) {
            if (i > 0) {
                sb.append(", ");
            }
            sb.append(args[i]);
        }
        sb.append(')');

        return sb;
    }

    private StringBuffer term(String functor, StringBuffer[] args) {
        StringBuffer sb = new StringBuffer();
        sb.append(functor);
        sb.append('(');
        for (int i = 0; i < args.length; i++) {
            if (i > 0) {
                sb.append(", ");
            }
            sb.append(args[i]);
        }
        sb.append(')');

        return sb;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#writeRule(java.lang.String,
     *           java.lang.String[], java.lang.String[])
     */
    public void writeRule(String functor, String[] args, String[] condi) {
    	StringBuffer buf = new StringBuffer();
        if (interpreted) {
            buf.append(INTE);
            buf.append("(");
        }
        buf.append(term(functor,args).toString());
        if (condi != null && condi.length > 0) {
        	buf.append(" :- ");
            for (int i = 0; i < condi.length; i++) {                    
                buf.append(",");
                buf.append(condi[i]);
            }
        }
       	buf.append(")");
       	clauses.add(buf.toString());
    }

//    /**
//     * @param string
//     */
//    private void write(String string)
//	{
//		try
//		{
//			out.write(string);
//		} catch (IOException e)
//		{
//			Debug.report(e);
//			throw new RuntimeException(e);
//		}
//
//	}

    /*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#setInterpretMode(boolean)
	 */
    public void setInterpretMode(boolean interpret) {
        this.interpreted = interpret;

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#getInterpretMode()
     */
    public boolean getInterpretMode() {
        return interpreted;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#writeQuery(java.lang.String)
     */
    public void writeQuery(String query) {
        clauses.add(query);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#close()
     */
    public void close() {
//       try {
//        out.close();
//    } catch (IOException e) {
//        Debug.report(e);
//        throw new RuntimeException(e);
//    }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.internal.astvisitor.IPrologWriter#flush()
     */
    public void flush() {
//        try {
//            out.flush();
//        } catch (IOException e) {
//            Debug.report(e);
//            throw new RuntimeException(e);
//        }

    }

}
