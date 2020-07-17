package com.wavesplatform.lang;

public class DApp implements Script {
    private final byte[] bytes;
    private final int version;
    final com.wavesplatform.lang.contract.DApp internal;

    DApp(byte[] bytes, int version, com.wavesplatform.lang.contract.DApp dApp) {
        this.bytes = bytes;
        this.version = version;
        this.internal = dApp;
    }

    @Override
    public int version() {
        return version;
    }

    @Override
    public boolean isExpression() {
        return false;
    }

    @Override
    public boolean isDApp() {
        return true;
    }

    @Override
    public boolean isAsset() {
        return false;
    }

    @Override
    public byte[] bytes() {
        return bytes;
    }
}
