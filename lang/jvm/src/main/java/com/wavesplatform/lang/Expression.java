package com.wavesplatform.lang;

import com.wavesplatform.lang.v1.compiler.Terms;

import java.util.Arrays;
import java.util.Objects;

public class Expression implements Script {
    private final byte[] bytes;
    private final int version;
    private final boolean isAsset;
    final Terms.EXPR internal;

    Expression(byte[] bytes, int version, boolean isAsset, Terms.EXPR expr) {
        this.bytes = bytes;
        this.version = version;
        this.isAsset = isAsset;
        this.internal = expr;
    }

    @Override
    public int version() {
        return version;
    }

    @Override
    public boolean isExpression() {
        return true;
    }

    @Override
    public boolean isDApp() {
        return false;
    }

    @Override
    public boolean isAsset() {
        return isAsset;
    }

    @Override
    public byte[] bytes() {
        return bytes;
    }

    @Override
    public String toString() {
        return "Expression{" +
                "bytes=" + Arrays.toString(bytes) +
                ", version=" + version +
                ", isAsset=" + isAsset +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Expression that = (Expression) o;
        return version == that.version &&
                isAsset == that.isAsset &&
                Arrays.equals(bytes, that.bytes);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(version, isAsset);
        result = 31 * result + Arrays.hashCode(bytes);
        return result;
    }
}
