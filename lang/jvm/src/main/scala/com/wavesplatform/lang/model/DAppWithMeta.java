package com.wavesplatform.lang.model;

import com.wavesplatform.lang.contract.DApp;

public class DAppWithMeta {
    private final DApp dApp;
    private final Meta meta;

    public DAppWithMeta(DApp dApp, Meta meta) {
        this.dApp = dApp;
        this.meta = meta;
    }

    public DApp getDApp() {
        return dApp;
    }

    public Meta getMeta() {
        return meta;
    }
}
