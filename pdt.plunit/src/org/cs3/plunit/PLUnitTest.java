package org.cs3.plunit;


import static org.junit.Assert.fail;

import java.io.File;
import java.net.URL;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.cs3.plunit.framework.AbstractPrologTestCase;
import org.cs3.plunit.framework.PrologFacade;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;

@RunWith(LabelledParameterized.class)
public class PLUnitTest extends AbstractPrologTestCase{

	private String testname;
	private String unit;
	private String assertionLine;


	public PLUnitTest(String arg,String unit,String testname,String file, String line) {
		// First argument is used by the TestRunner to add a meaningful testname 
		this.testname = testname;
		this.unit= unit;
	}

	@SuppressWarnings("rawtypes")
	@Parameters(name="{0}:{3}:{4}")
	public static Collection data() throws Exception{
		
//		prepareTests();

		List<Map<String, Object>>  tests = PrologFacade.queryAll("junitadapter:unit_test(Unit,Test,File,Line)");
	
		if(tests.isEmpty())
			throw new Exception("No testcases found");
		
		Object[][] data=new String[tests.size()][5];
		for(int i=0;i<tests.size();i++){
			String unitname=tests.get(i).get("Unit").toString();
			String testname=tests.get(i).get("Test").toString();
			String file=tests.get(i).get("File").toString();
			String line=tests.get(i).get("Line").toString();
			data[i][0]=unitname+":"+testname;
			data[i][1]=unitname;
			data[i][2]=testname;
			data[i][3]=file.replace(':', '|');
			data[i][4]=line;
			
		}

		return Arrays.asList(data);
	}


	@Test
	public void lpane() throws Exception{
		
		boolean print = true;
		TimeMeasurement fullTm = new TimeMeasurement("test: " + testname, print );

		PrologFacade.queryAll("run_tests("+unit+":"+testname+")");


		Map<String,Object> failed=PrologFacade.queryOnce("plunit:failed(_,_,Line,Error)");
		if(failed != null){
			fail("Failed in Line "+failed.get("Line")+" with Error "+failed.get("Error")+" of "+unit+":"+testname);
		}
		
		Map<String,Object> blocked=PrologFacade.queryOnce("plunit:blocked(_,_,Line,Reason)");
		if(blocked != null){
			fail("Blocked in Line "+blocked.get("Line")+" with "+blocked.get("Reason")+" of "+unit+":"+testname);
		}

		Map<String,Object> failedAssertion=PrologFacade.queryOnce("plunit:failed_assertion(Unit, Test, Line, File:ALine, STO, Reason,Module:Goal)");
		if(failedAssertion != null){
			assertionLine = (String)failedAssertion.get("ALine");
			fail("Failed Assertion in Line "+failedAssertion.get("ALine")+" with "+failedAssertion.get("Reason")+" of goal " + failedAssertion.get("Module")+":"+ failedAssertion.get("Goal")+"\n   test case: "+unit+":"+testname);
		}

		fullTm.getTimeDiff();
		
	}

}
