package com.wavesplatform.lang;

import com.wavesplatform.lang.v1.compiler.Terms;

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
}
