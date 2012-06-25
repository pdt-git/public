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


	public PLUnitTest(String arg,String unit,String testname,String file, String line) {
		// First argument is used by the TestRunner to add a meaningful testname 
		this.testname = testname;
		this.unit= unit;
	}

	@SuppressWarnings("rawtypes")
	//@Parameters
	@Parameters(name="{2}:{4}")
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
			data[i][3]=file;
			data[i][4]=line;
			
		}

		return Arrays.asList(data);
	}


	@Test
	public void _() throws Exception{
		
		Map<String,Object> failed = PrologFacade.queryOnce(
				"forall(" + // ensure all choice points processed
				"run_tests("+unit+":"+testname+")," +
						"true)," +
				"junitadapter:test_failure(Kind,Msg,Line)");
		if(failed != null){
			fail(""+failed.get("Msg"));
		}
		
	}

}
