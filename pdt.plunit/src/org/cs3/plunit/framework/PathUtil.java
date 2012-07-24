/* $LICENSE_MSG$ */

package org.cs3.plunit.framework;

import java.io.File;

public class PathUtil {

	public static String getProjectPath(Class c){
		String path = stripBinaryPath(getBinPath(c));
		return path.replaceFirst(".test", "");
	}

	public static String getPath(Class c){
		return stripBinaryPath(getBinPath(c));
	}
	
	public static String getBinPath(Class c) {
		String classPath = c.getResource(".").getPath();
		String packagePath = c.getPackage().getName().replace(".", "/");
		String binPath = classPath.replace(packagePath, "");
		return binPath;
	}
	
	private static String stripBinaryPath(String path) {
	    return path.replace("/bin/", "")
                   .replace("/target/classes", "")
                   .replace("/target/test-classes", "");
	}
}

