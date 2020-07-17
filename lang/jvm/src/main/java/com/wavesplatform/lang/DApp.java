package com.wavesplatform.lang;

import java.util.Arrays;
import java.util.Objects;

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

    @Override
    public String toString() {
        return "DApp{" +
                "bytes=" + Arrays.toString(bytes) +
                ", version=" + version +
                ", internal=" + internal +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DApp dApp = (DApp) o;
        return version == dApp.version &&
                Arrays.equals(bytes, dApp.bytes);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(version);
        result = 31 * result + Arrays.hashCode(bytes);
        return result;
    }
}
