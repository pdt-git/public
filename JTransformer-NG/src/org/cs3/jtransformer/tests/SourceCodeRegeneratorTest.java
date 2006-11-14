/*
 * Created on 11.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.jtransformer.tests;

import java.io.IOException;

import org.cs3.jtransformer.regenerator.IAffectedFile;
import org.cs3.jtransformer.regenerator.ISourceRegenerator;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

/**
 * @author trho
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class SourceCodeRegeneratorTest extends FactGenerationTest {
	public void setUpOnce() throws Exception {
		setAutoBuilding(false);
		super.setUpOnce();
	}
	/**
     * @param name
     */
    public SourceCodeRegeneratorTest(String name) {
        super(name);        
    }

    /**
	 * @throws IOException
     * @throws PrologInterfaceException 
	 * 
	 */
	public void testRegeneration() throws IOException, PrologInterfaceException {
		PrologInterface pif = getTestJTransformerProject().getPrologInterface();
		PrologSession session = pif.getSession();
		try{
			session.queryOnce("win_window_pos([show(true)])");

			
	    String pred =
	    		
		"assert_tree(toplevelT(t3,null,'/Project1/src/class3.java',[class3])),"+
		"assert_tree(projectLocationT(t3,'Project1',src)),"+
		"assert_tree(projectT('Project1','c:/eclipse/ws/Project1','Project1-output','c:/eclipse/ws/Project1-output')),"+
		"assert_tree(modifierT(class3,public)),"+
		"assert_tree(classDefT(class3,null,cname3,[method4])),"+
	    "assert_tree(methodDefT(method4,class2,name4,[],type(basic,int,0),[],null)),"+
		"action(delete(class(class3,null,cname3))),"+
		
		
			"action(add(modifierT(class,public)))," +
				"action(add(class(class,null,cname))),"+
	    "action(add(method(method1,class,name1,[],type(basic,int,0),[],null))),"+
	    "action(add(method(method2,class,name2,[],type(basic,int,0),[],null))),"+
//	    "action(add(dirty_tree(method1))),"+
//	    "action(add(dirty_tree(method2))),"+

		"assert_tree(toplevelT(tl,null,filename2,[class2])),"+
		"assert_tree(classDefT(class2,null,cname2,[])),"+
	    "action(add(method(method3,class2,name3,[],type(basic,int,0),[],null))).";

		
//	    "action(add(dirty_tree(method3)))";		
	    
		session.queryOnce(pred);
		//session.queryOnce("prolog_server(1080,[])");
		
		ISourceRegenerator regen = getTestJTransformerProject().getSourceRegenerator();
		IAffectedFile[] files= regen.getAffectedFiles();
		assertEquals(3,files.length);
		assertEquals(IAffectedFile.REMOVED,files[0].getStatus());
		assertEquals(IAffectedFile.CREATED,files[1].getStatus());
		assertEquals(IAffectedFile.CHANGED,files[2].getStatus());
			
		session.queryOnce("rollback");
		
		IAffectedFile[] files2= regen.getAffectedFiles();
		assertEquals(3,files2.length);
		session.queryOnce(
				"retract_tree(toplevelT(t3,null,'/Project1/src/class3.java',[class3])),"+
				"retract_tree(projectLocationT(t3,'Project1',src)),"+
				"retract_tree(projectT('Project1','c:/eclipse/ws/Project1','Project1-output','c:/eclipse/ws/Project1-output')),"+
				"retract_tree(modifierT(class3,public)),"+
				"retract_tree(classDefT(class3,null,cname3,[method4])),"+
			    "retract_tree(methodDefT(method4,class2,name4,[],type(basic,int,0),[],null)),"+
				"retract_tree(toplevelT(tl,null,filename2,[class2])),"+
				"retract_tree(classDefT(class2,null,cname2,[]))"
				);

		}
		finally{
		    session.dispose();
		}
				
	}
	
}
