/*
 * DocWriter.java
 *
 * Created on 15. Dezember 2001, 20:08
 */

package org.cs3.pl.doc;

import java.io.IOException;
import java.util.HashMap;

import org.cs3.pl.common.Debug;
import org.cs3.pl.fileops.MetaDataManager;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.prolog.SessionException;

/**
 *
 * @author  windeln
 */
public class PrologDocWriter extends HTMLWriter {


    String dir;
    
//    public void write(Library tree) {
//        File mkdir = new File(dir + tree.name);
//        mkdir.mkdir();
//        openStream(dir + tree.fileName());
//        writeHead(tree, "Libraries");
//        writeComment(tree);
//        writeUses(tree, "Uses");
//        if (tree.classes.size() > 0)
//  // write classes Summary        
//            writeSection(false, true, "Classes", tree.classes);
//        if (tree.defs.size() > 0) {
//  // write methods Summary and Detail            
//            writeSection(false, true, "Method", tree.defs);             
//            writeSection(false, false, "Method", tree.defs);
//        }
//        writeTail();
//        closeStream();
//        openStream(dir + tree.frameFileName());
//        writeHead(tree.name, "Library-Frame");
//        writeFrame(tree);
//        writeTail();
//        closeStream();
//    }
    
//    public void writeUses(Library lib, String Title) {
//        if (lib.uses.size() > 0) {
//            writeStream("<DL>\n"+ 
//                        "<DT><B>"+ Title + ":</B>\n<FONT SIZE=\"-1\">\n");
//            for (int i = 0; i < lib.uses.size(); i++) {
//                String libName = ((Use)lib.uses.get(i)).libName;
//                Library l = SymTab.getLibrary(libName);
//                writeStream("<DD><A HREF="+ moveInDirectoryHierachy+l.fileName()+ ">" + libName +"</A></DD>\n");
//            }
//            writeStream("</DL></FONT><HR>\n");
//        }
//    }
    
    MetaDataManager manager ;
    

//    public void writeAllClassesFrame(LibraryTypes types) {
//        Vector libs = types.getAllElements();
//        this.openStream(dir + "allclasses-frame.html");
//        writeDefaultHeader("All Classes");
//        this.writeStream(
//            "<FONT size=\"+1\" CLASS=\"FrameHeadingFont\">\n"+
//            "<B>All Classes</B></FONT>\n"+
//            "<BR>\n"+
//            "<TABLE BORDER=\"0\" WIDTH=\"100%\">\n"+
//            "<TR>\n"+
//            "<TD NOWRAP><FONT CLASS=\"FrameItemFont\">\n");
//        
//        for(int i = 0; i< libs.size(); i++) {
//            Library lib = (Library)libs.get(i);
//            for(int j = 0; j < lib.classes.size(); j++) {
//                Clazz clazz = (Clazz)lib.classes.get(j);
//                this.writeStream(
//                    "<A HREF=\""+ clazz.fileName() + "\" TARGET=\"classFrame\">" + 
//                    clazz.name + "</A>\n" +
//                    "<BR>\n");
//
//            }
//        }
//        this.writeStream(
//            "</FONT></TD>\n"+
//            "</TR>\n"+
//            "</TABLE>\n");
//        writeTail();
//        closeStream();
//    }

    String moveInDirectoryHierachy = "..\\";
   
    /** Creates a new instance of DocWriter */
    public PrologDocWriter(MetaDataManager manager,String targetdir) {
        super("stylesheet.css");
        this.dir = targetdir;
        this.manager=manager;
    }

    public String getDocumentationFile(PrologModule module) {
    	return manager.getFullPath(module.getFilename());
    }

    public void write(PrologModule module) throws IOException {
//    	File file = new File(dir + module.getHelpFilename());
//    	file.getParentFile().mkdirs();
//        openStream(new FileOutputStream(file));
        
        setWriter(manager.getMetaDataWriter(module.getFilename()));
        writeHead("Module");
        writeComment(module);
        writeSection(true, "Predicates", module.getElements()); 
        writeSection(false, "Predicates",  module.getElements());

        writeTail();
        close();
    }




	public void writeComment(PrologModule module) throws IOException {
		String help = module.getHelp();
		help = (help == null) ? "" : help;
        write(
            "<HR>\n"+
            "<!-- ======== START OF MODULE DATA ======== -->\n"+
            "<H2>\n"+
            "<FONT SIZE=\"-1\">\n"+
            "Module " + module.getName() + "</FONT>\n"+
            "<BR>\n"+
            module.getName() + "</H2>\n"+
            help+"\n"+
            "<BR>\n");
    }

    void writeDetailEntry(PrologElementData data) throws IOException {
    	String help = null;
    	try {
            help=data.getHelp();
        } catch (SessionException e) {
            Debug.report(e);
        }
    	if (help == null)
    		help = "";
        write(
            "\n<!-- DETAIL ENTRY -->\n"+
            "<A NAME=\""+data.getSignature()+ "\"><!-- --></A>\n" +
            "<H3>"+ data.getSignature() +"</H3>\n"+
            "<DL>\n"+
            "<DD><PRE>" +help+"</PRE>\n"+
            "<P><DD><DL>\n");
        write(
            "</DD>\n"+
            "</DL>\n"+
            "</DL>\n"+
            "<HR>\n");
    }

    
    public void writeHead(String module) throws IOException {
        super.writeHead(module);
        write(        
            "<!-- ========== START OF NAVBAR ========== -->\n"+
            "<A NAME=\"navbar_top\"><!-- --></A>\n"+
            "<TABLE BORDER=\"0\" WIDTH=\"100%\" CELLPADDING=\"1\" CELLSPACING=\"0\">\n"+
            "<TR>\n"+
            "<TD COLSPAN=2 BGCOLOR=\"#EEEEFF\" CLASS=\"NavBarCell1\">\n"+
            "<A NAME=\"navbar_top_firstrow\"><!-- --></A>\n"+
            "<TABLE BORDER=\"0\" CELLPADDING=\"0\" CELLSPACING=\"3\">\n"+
            "<TR ALIGN=\"center\" VALIGN=\"top\">\n"+
            "<TD BGCOLOR=\"#EEEEFF\" CLASS=\"NavBarCell1\">    <A HREF=\""+moveInDirectoryHierachy+ "overview-summary.html\"><FONT ID=\"NavBarFont1\"><B>Overview</B></FONT></A>&nbsp;</TD>\n"+
            "<TD BGCOLOR=\"#EEEEFF\" CLASS=\"NavBarCell1\">    <A HREF=\""+moveInDirectoryHierachy + module +".pl"+ "\"><FONT ID=\"NavBarFont1\"><B>Library</B></FONT></A>&nbsp;</TD>\n"+
            "</TR>\n"+
            "</TABLE>\n"+
            "</TD>\n"+
            "<TD ALIGN=\"right\" VALIGN=\"top\" ROWSPAN=3><EM>\n"+
            "</EM>\n"+
            "</TD>\n"+
            "</TR>\n"+
            "</TABLE>\n");

    }
//    
//    public Sub getConstructor(Vector defs) {
//        for (int i = 0; i < defs.size(); i++){
//            if (defs.get(i) instanceof Sub) {
//                Sub sub = (Sub)defs.get(i);
//                if (sub.name.equalsIgnoreCase("new"))
//                    return sub;
//            }
//        }
//        return null;
//    }
    

    
        
    
    
//    String paramsToString(Sub sub) {
//        String sig = sub.signature;
//        int indexBrace = sig.indexOf('(');
//        if (indexBrace > 0)
//            return sig.substring(indexBrace, sig.length());
//        else
//            return "";
//    }
    
    
    void writeSection(boolean bSummary, String title, PrologElementData[] elements) throws IOException {
        if (bSummary)
            writeTableHead(title, "Summary");
        else {
            writeTableHead(title, "Detail");
            write("</TABLE>");
        }
        HashMap map = new HashMap();
        for (int i = 0; i < elements.length; i++) {
        	String sig = elements[i].getSignature();
        	if (!map.containsKey(sig)) {
        		map.put(sig,sig);
				if (bSummary) {
					writeSummaryEntry(elements[i]);
				} else
					writeDetailEntry(elements[i]);
        	}
		}
		if (bSummary)
            write("</TABLE>");

    }
    
    public void writeSummaryEntry(PrologElementData data) throws IOException {
        String summary=null;
        try {
            summary = data.getSummary();
        } catch (SessionException e) {
            Debug.report(e);
        }
        if (summary == null)
        	summary = "";
       writeTableRow(
            "<TD ALIGN=\"right\" VALIGN=\"top\" WIDTH=\"1%\"></TD>\n"+ 
            "<TD><CODE><B><A HREF=\"#" + data.getSignature() + "\">" + data.getSignature() + "</A></B></CODE>\n"+ 
            "<BR>\n"+ summary + "\n" +
            "</TD>\n");
    }

   
    public void writeTableRow(String row) throws IOException {
        write(
            "<TR BGCOLOR=\"white\" CLASS=\"TableRowColor\">\n"+ 
            row + 
            "</TR>\n");
    }
    
//    public void writeOverviewSummary(LibraryTypes types) {
//        openStream(dir + "overview-summary.html");
//        writeHead("Overview", "Library Summary");
//  // write library Summary 
//        moveInDirectoryHierachy = "";
//        for (int i= 0 ;i<types.all.libs.size(); i++) {
//            ScriptPackage pkg = (ScriptPackage)types.all.libs.get(i);
//            writeSection(false, true, pkg.name, pkg.libs);
//        }
//        moveInDirectoryHierachy = "..\\";
//        writeTail();
//        closeStream();
//        
//    }
    
//    public void writeOverviewFrame(LibraryTypes types) {
//        this.openStream(dir + "overview-frame.html");
//        writeDefaultHeader(": Overview");
//        this.writeStream(
//            "<TABLE BORDER=\"0\" WIDTH=\"100%\">\n"+
//            "<TR>\n"+
//            "<TD NOWRAP><FONT size=\"+1\" CLASS=\"FrameTitleFont\">\n"+
//            "<B></B></FONT></TD>\n"+
//            "</TR>\n"+
//            "</TABLE>\n"+
//            "<TABLE BORDER=\"0\" WIDTH=\"100%\">\n"+
//            "<TR>\n"+
//            "<TD NOWRAP><FONT CLASS=\"FrameItemFont\"><A HREF=\"allclasses-frame.html\" TARGET=\"libraryFrame\">All Classes</A></FONT>\n"+
//            "<P>\n");
//        for(int i = 0; i< types.all.libs.size(); i++) {
//            ScriptPackage pkg = (ScriptPackage)types.all.libs.get(i);
//            writeOverviewFrameType(pkg.name, pkg.libs);
//        }
//        this.writeStream(
//            "</TD>\n"+
//            "</TR>\n"+
//            "</TABLE>\n"+
//            "</BODY>\n"+
//            "</HTML>\n");
//        this.closeStream();        
//    }
//
//    void writeOverviewFrameType(String name, Vector libs) {
//        this.writeStream(        
//            "<FONT size=\"+1\" CLASS=\"FrameHeadingFont\">\n"+
//            name+"</FONT>\n"+
//            "<BR>\n");
//        for(int i = 0; i< libs.size(); i++) {
//            Library lib = (Library)libs.get(i);
//            this.writeStream(
//                "<FONT CLASS=\"FrameItemFont\"><A HREF=\""+lib.frameFileName()+"\" TARGET=\"libraryFrame\">"+lib.name+"</A></FONT>\n"+
//                "<BR>\n");
//        }
//    }
    
//    void writeFrame(PrologModule module) {
//        this.writeStream(
//            "<FONT size=\"+1\" CLASS=\"FrameTitleFont\">\n" + 
//            "<A HREF=\"" + moveInDirectoryHierachy + module.getFilename() + "\" TARGET=\"classFrame\">" + module.getName() + "</A></FONT>\n" + 
//            "<TABLE BORDER=\"0\" WIDTH=\"100%\">\n" + 
//            "<TR>\n" + 
//            "<TD NOWRAP><FONT size=\"+1\" CLASS=\"FrameHeadingFont\">\n" + 
//            "Classes</FONT>&nbsp;\n" + 
//            "<FONT CLASS=\"FrameItemFont\">\n");
//        for (int i = 0;i< module.getElements().length; i++) {
//            Clazz clazz = module.getElements().get(i);
//            this.writeStream(
//                "<BR>\n" +
//                "<A HREF=\""+ moveInDirectoryHierachy+clazz.fileName() + "\" TARGET=\"classFrame\">" + clazz.name + "</A>\n");
//         }
//         this.writeStream(
//            "</FONT></TD>\n" + 
//            "</TR>\n" + 
//            "</TABLE>\n");
//    }

}
