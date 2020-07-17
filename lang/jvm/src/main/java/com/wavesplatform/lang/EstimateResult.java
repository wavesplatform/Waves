package com.wavesplatform.lang;

import java.util.Map;

public class EstimateResult {
    private final int complexity;
    private final Map<String, Integer> callableComplexities;

    public EstimateResult(int complexity, Map<String, Integer> callableComplexities) {
        this.complexity = complexity;
        this.callableComplexities = callableComplexities;
    }

    @Override
    public String toString() {
        return "EstimateResult{" +
                "complexity=" + complexity +
                ", callableComplexities=" + callableComplexities +
                '}';
    }

    public int getComplexity() {
        return complexity;
    }

    public Map<String, Integer> getCallableComplexities() {
        return callableComplexities;
    }
}
