package com.wavesplatform.lang;

import com.wavesplatform.lang.v1.repl.Repl;
import scala.concurrent.Await;
import scala.concurrent.Future;
import scala.concurrent.duration.FiniteDuration;
import scala.util.Either;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class RideRepl {
    private final Repl internal;

    public RideRepl(Repl internal) {
        this.internal = internal;
    }

    public void clear() {
        internal.clear();
    }

    public String info(String text) {
        return internal.info(text);
    }

    public String totalInfo() {
        return internal.totalInfo();
    }

    public String execute(String expression) {
        Future<Either<String, String>> execution = internal.execute(expression);
        try {
            return Await.result(execution, new FiniteDuration(5, TimeUnit.SECONDS))
                    .fold(error -> { throw new RideException(error); }, r -> r);
        } catch (InterruptedException | TimeoutException e) {
            throw new RideException(e);
        }
    }
}
