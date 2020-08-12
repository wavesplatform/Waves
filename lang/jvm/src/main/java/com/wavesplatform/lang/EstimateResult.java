package com.wavesplatform.lang;

import java.util.Map;

public class EstimateResult {
    private final int verifierComplexity;
    private final int maxComplexity;
    private final Map<String, Integer> callableComplexities;

    public EstimateResult(int verifierComplexity, int maxComplexity, Map<String, Integer> callableComplexities) {
        this.verifierComplexity = verifierComplexity;
        this.maxComplexity = maxComplexity;
        this.callableComplexities = callableComplexities;
    }

    @Override
    public String toString() {
        return "EstimateResult{" +
                "complexity=" + verifierComplexity +
                ", callableComplexities=" + callableComplexities +
                '}';
    }

    public int getVerifierComplexity() {
        return verifierComplexity;
    }

    public Map<String, Integer> getCallableComplexities() {
        return callableComplexities;
    }

    public int getMaxComplexity() {
        return maxComplexity;
    }
}
