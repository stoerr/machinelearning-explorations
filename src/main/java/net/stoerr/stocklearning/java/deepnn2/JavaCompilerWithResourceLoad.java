package net.stoerr.stocklearning.java.deepnn2;

import javax.tools.*;
import java.io.File;
import java.io.Writer;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.Random;

/**
 * Uses the standard java compiler and enables loading of the class as stream
 *
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 05.10.2015
 */
public class JavaCompilerWithResourceLoad {

    private static final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
    private static final StandardJavaFileManager fm = compiler.getStandardFileManager(null, null, null);
    private static final File generatedClassDirectory = new File("target/generated");
    private static final URLClassLoader classLoader;
    private static final JavaFileManager.Location generatedLocation = new JavaFileManager.Location() {
        @Override
        public String getName() {
            return "generated";
        }

        @Override
        public boolean isOutputLocation() {
            return true;
        }
    };

    static {
        try {
            generatedClassDirectory.mkdir();
            classLoader = new URLClassLoader(new URL[]{generatedClassDirectory.toURI().toURL()}, JavaCompilerWithResourceLoad.class.getClassLoader());
            fm.setLocation(generatedLocation, Arrays.asList(generatedClassDirectory));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static <T> Class<T> compile(String classname, String code) {
        try {
            JavaFileObject fo = fm.getJavaFileForOutput(generatedLocation, classname, JavaFileObject.Kind.SOURCE, null);
            Writer writer = fo.openWriter();
            writer.write(code);
            writer.close();
            boolean ok = compiler.getTask(null, fm, null, Arrays.asList("-g"), null, Arrays.asList(fo)).call();
            if (!ok) throw new IllegalStateException();
            return (Class<T>) classLoader.loadClass(classname);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }


    public static void main(String[] args) throws Exception {
        String classname = "GeneratedTest" + Math.abs(new Random().nextLong());
        String code = "public class " + classname + " { public static void main(String[] args) { System.out.println(\"Hello!\"); }}";
        System.out.println(code);
        compile(classname, code);
        Class<?> clazz = classLoader.loadClass(classname);
        System.out.println(clazz);
        System.out.println(classLoader.getResourceAsStream(classname + ".class"));
    }

}
