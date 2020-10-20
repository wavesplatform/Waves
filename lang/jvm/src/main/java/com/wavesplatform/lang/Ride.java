package com.wavesplatform.lang;

import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings;

public class Ride {
    public static Script compile(String program) {
        return JavaAdapter.compile(program);
    }

    public static String decompile(Script script) {
        return JavaAdapter.decompile(script);
    }

    public static EstimateResult estimate(Script script) {
        return JavaAdapter.estimate(script);
    }

    public static ScriptMeta meta(Script script) {
        return JavaAdapter.extractMeta(script);
    }
}
