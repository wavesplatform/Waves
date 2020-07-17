package com.wavesplatform.lang;

import java.util.List;

public class ScriptMeta {
    private final int version;
    private final List<FunctionSignature> functionSignatures;

    public ScriptMeta(int version, List<FunctionSignature> functionSignatures) {
        this.version = version;
        this.functionSignatures = functionSignatures;
    }

    public int getVersion() {
        return version;
    }

    public List<FunctionSignature> getFunctionSignatures() {
        return functionSignatures;
    }

    public static class FunctionSignature {
        private final String name;
        private final List<FunctionArgument> arguments;

        public FunctionSignature(String name, List<FunctionArgument> arguments) {
            this.name = name;
            this.arguments = arguments;
        }

        public String getName() {
            return name;
        }

        public List<FunctionArgument> getArguments() {
            return arguments;
        }
    }

    public static class FunctionArgument {
        private final String name;
        private final String type;

        public FunctionArgument(String name, String type) {
            this.name = name;
            this.type = type;
        }

        public String getName() {
            return name;
        }

        public String getType() {
            return type;
        }
    }
}
