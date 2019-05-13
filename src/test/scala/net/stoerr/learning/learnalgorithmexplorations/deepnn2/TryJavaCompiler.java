package net.stoerr.learning.learnalgorithmexplorations.deepnn2;

import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import java.io.File;
import java.util.Arrays;

/**
 * Attempt to use the JavaCompiler interface
 *
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @see "https://docs.oracle.com/javase/8/docs/api/javax/tools/JavaCompiler.html"
 * @since 28.09.2015
 */
public class TryJavaCompiler {

    public static void main(String[] args) throws Exception {
        File tmpJava = File.createTempFile("TryJavaCompilerRun", ".java");
        tmpJava.deleteOnExit();
        String classname = tmpJava.getName().replaceFirst("\\.java$", "");
        String code = "package test; public class " + classname + " { public static void main(String[] args) { System.out.println(\"Hello!\"); }";
        System.out.println(code);
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        StandardJavaFileManager fm = compiler.getStandardFileManager(null, null, null);
        JavaFileObject javaFileObject = fm.getJavaFileObjects(tmpJava).iterator().next();
        boolean ok = compiler.getTask(null, fm, null, null, null, Arrays.asList(javaFileObject)).call();
        assert ok;
        System.out.println(javaFileObject);
        // Class<?> clazz = TryJavaCompiler.class.getClassLoader().loadClass("test." + classname);
        // System.out.println(clazz);
    }

}
