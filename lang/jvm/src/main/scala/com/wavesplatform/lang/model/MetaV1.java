package com.wavesplatform.lang.model;

import com.wavesplatform.lang.contract.meta.FunctionSignatures;

public class MetaV1 implements Meta {
    private final FunctionSignatures functionSignatures;

    public MetaV1(FunctionSignatures functionSignatures) {
        this.functionSignatures = functionSignatures;
    }

    public FunctionSignatures getFunctionSignatures() {
        return functionSignatures;
    }
}
