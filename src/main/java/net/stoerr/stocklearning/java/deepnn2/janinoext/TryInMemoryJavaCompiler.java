package net.stoerr.stocklearning.java.deepnn2.janinoext;

import org.mdkt.compiler.DynamicClassLoader;
import org.mdkt.compiler.InMemoryJavaCompiler;

import java.util.Arrays;

/**
 * Tries out https://github.com/trung/InMemoryJavaCompiler
 *
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 28.09.2015
 */
public class TryInMemoryJavaCompiler {

    public static void main(String[] args) throws Exception {
        String code = "package test; public class TryInMemTest extends " + TryInMemoryJavaCompiler.class.getName()
                + "{ public static void main(String[] args) { System.out.println(\"Hello!\"); }}";
        Class<?> helloTest = InMemoryJavaCompiler.compile("test.TryInMemTest", code);
        System.out.println(Arrays.asList(helloTest.getDeclaredMethods()));
        helloTest.getMethod("main", String[].class).invoke(null, new String[1]);
        DynamicClassLoader cl = (DynamicClassLoader) helloTest.getClassLoader();
        System.out.println(cl);
        System.out.println(cl.getResourceAsStream("/test/TryInMemTest.class"));
    }
}
