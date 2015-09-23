package net.stoerr.stocklearning.java.deepnn2.janinoext;

import org.codehaus.janino.ByteArrayClassLoader;
import org.codehaus.janino.SimpleCompiler;

import java.lang.reflect.Field;
import java.util.Map;

/**
 * Created by hps on 23.09.2015.
 */
public class SimpleCompilerWithResourceLoad extends SimpleCompiler {

    private ClassLoader fixedResult = null;

    @Override
    public ClassLoader getClassLoader() {
        if (null == fixedResult) {
            try {
                Field resultField = SimpleCompiler.class.getDeclaredField("result");
                resultField.setAccessible(true);
                ByteArrayClassLoader result = (ByteArrayClassLoader) resultField.get(this);
                Field classesField = ByteArrayClassLoader.class.getDeclaredField("classes");
                classesField.setAccessible(true);
                Map<String /*className*/, byte[] /*data*/> classes = (Map<String /*className*/, byte[] /*data*/>) classesField.get(result);
                fixedResult = new ByteArrayClassLoaderWithResourceLoad(classes);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return fixedResult;
    }

}
