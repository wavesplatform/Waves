package com.wavesplatform.lang.model;

import java.util.List;
import java.util.Map;

public class MetaV1 implements Meta {
    private final Map<String, List<ArgNameWithType>> functionSignatures;

    public MetaV1(Map<String, List<ArgNameWithType>> functionSignatures) {
        this.functionSignatures = functionSignatures;
    }

    public Map<String, List<ArgNameWithType>> getFunctionSignatures() {
        return functionSignatures;
    }
}
