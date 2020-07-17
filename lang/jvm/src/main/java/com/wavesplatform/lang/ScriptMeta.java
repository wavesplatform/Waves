package com.wavesplatform.lang;

import java.util.List;
import java.util.Objects;

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

    @Override
    public String toString() {
        return "ScriptMeta{" +
                "version=" + version +
                ", functionSignatures=" + functionSignatures +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ScriptMeta that = (ScriptMeta) o;
        return version == that.version &&
                Objects.equals(functionSignatures, that.functionSignatures);
    }

    @Override
    public int hashCode() {
        return Objects.hash(version, functionSignatures);
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

        @Override
        public String toString() {
            return "FunctionSignature{" +
                    "name='" + name + '\'' +
                    ", arguments=" + arguments +
                    '}';
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            FunctionSignature that = (FunctionSignature) o;
            return Objects.equals(name, that.name) &&
                    Objects.equals(arguments, that.arguments);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name, arguments);
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

        @Override
        public String toString() {
            return "FunctionArgument{" +
                    "name='" + name + '\'' +
                    ", type='" + type + '\'' +
                    '}';
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            FunctionArgument that = (FunctionArgument) o;
            return Objects.equals(name, that.name) &&
                    Objects.equals(type, that.type);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name, type);
        }
    }
}
