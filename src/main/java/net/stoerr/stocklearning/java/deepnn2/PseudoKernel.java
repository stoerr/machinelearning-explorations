package net.stoerr.stocklearning.java.deepnn2;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * Something vaguely like Aparapi
 */
public abstract class PseudoKernel {

    /**
     * Override with the actual execution, using #getGlobalId to identify the exact work to be done.
     */
    abstract public void run();

    /**
     * The identifier that leads us to the actual part of work being done.
     */
    public int getGlobalId() {
        return currentId.get();
    }

    private ThreadLocal<Integer> currentId = new ThreadLocal<>();

    private static class LazyInit {
        private static final ExecutorService executor = Executors.newWorkStealingPool();
    }

    public void execute(int maxId) throws Throwable {
        /* for (int i = 0; i < maxId; ++i) {
            currentId.set(i);
            run();
            currentId.remove();
        } */

        List<Future<Void>> results = new ArrayList<>();
        for (int i = 0; i < maxId; ++i) {
            final int theId = i; // Java is stooopid.
            results.add(LazyInit.executor.submit(() -> {
                currentId.set(theId);
                try {
                    run();
                } finally {
                    currentId.remove();
                }
                return null;
            }));
        }
        try {
            for (Future<Void> future : results) future.get();
        } catch (ExecutionException e) {
            throw e.getCause();
        }
    }

}
