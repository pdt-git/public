/* $LICENSE_MSG$(ld) */

/*
 * HTMLWriter.java
 *
 * Created on 2. Januar 2002, 00:20
 */

package org.cs3.pl.doc;

import java.io.IOException;
import java.io.Writer;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;
/**
 *
 * @author  windeln
 */
public class HTMLWriter {

    public String styleSheetPath = "";
	private Writer writer;
    
    /** Creates a new instance of HTMLWriter */
    public HTMLWriter(String styleSheetPath) {
        this.styleSheetPath = styleSheetPath;
    }
    
//    /**
//	 * @param string
//	 */
//	protected void openStream(OutputStream out) {
//    	writer = new OutputStreamWriter(out);
//	}

	protected void close() throws IOException {
    	writer.close();
	}
    
    void writeTableHead(String type, String name) throws IOException {
        write(
            "<!-- ========== " + type.toUpperCase() + " " + name.toUpperCase() + " =========== -->\n"+ 
            "<A NAME=\"" + type.toLowerCase() + "_" + name.toLowerCase() + "\"><!-- --></A>\n"+ 
            "<TABLE BORDER=\"0\" CELLPADDING=\"3\" CELLSPACING=\"0\" WIDTH=\"100%\">\n"+ 
            "<TR BGCOLOR=\"#CCCCFF\" CLASS=\"TableHeadingColor\">\n"+ 
            "<TD COLSPAN=2><FONT SIZE=\"+1\">\n"+ 
            type + " " + name + "</FONT></TD>\n"+ 
            "</TR>\n");
    }

    void writeHeading(String heading, int num) throws IOException {
        write("<H" + num + "> " + heading + "</H" + num + ">\n");
    }
    
    void writeHeading(String heading) throws IOException {
        writeHeading(heading, 1);
    }
     
    void writeDefaultHeader(String title) throws IOException {
        write(
            "<HTML>\n" + 
            "<HEAD>\n" + 
            "<!-- Generated by prolog documentation (by Tobias Rho) on " + getCurrentTimeDate() + "-->\n" + 
            "<TITLE>\n" + 
            ": " + title + "\n" + 
            "</TITLE>\n" + 
            "<LINK REL =\"stylesheet\" TYPE=\"text/css\" HREF=\"" + styleSheetPath + "\" TITLE=\"Style\">\n" + 
            "</HEAD>\n" +
            "<BODY BGCOLOR=\"white\">\n");
    }
    
    
    void writeHead(String name) throws IOException {
        writeDefaultHeader(name);
    }        
    
    void writeTail() throws IOException {
        write("</BODY>\n"+
                    "</HTML>\n");
    }    
    
    protected void write(String str) throws IOException {
    	writer.write(str);
    }
    
    String getCurrentTimeDate() {
        GregorianCalendar c = new GregorianCalendar();
        SimpleDateFormat df = new SimpleDateFormat();
        return df.format(c.getTime());
    }

	/**
	 * @param metaDataWriter
	 */
	protected void setWriter(Writer metaDataWriter) {
		writer = metaDataWriter;
		
	}

}

