package com.wavesplatform.lang;

import com.wavesplatform.lang.directives.values.StdLibVersion;
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings;

public class Ride {
    public static Script compile(String program) {
        return JavaAdapter.compile(program);
    }

    public static String decompile(Script script) {
        return JavaAdapter.decompile(script);
    }

    public static int estimate(Script script) {
        return JavaAdapter.estimate(script);
    }

    public static ScriptMeta meta(Script script) {
        return JavaAdapter.extractMeta(script);
    }

    public static RideRepl repl(StdLibVersion stdLibVersion) {
        return JavaAdapter.repl(stdLibVersion);
    }

    public static RideRepl repl(StdLibVersion stdLibVersion, NodeConnectionSettings settings) {
        return JavaAdapter.repl(stdLibVersion, settings);
    }
}
