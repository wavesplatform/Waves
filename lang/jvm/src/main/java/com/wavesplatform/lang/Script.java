package com.wavesplatform.lang;

import com.wavesplatform.common.utils.Base64;

public interface Script {
    int version();

    boolean isExpression();

    boolean isDApp();

    boolean isAsset();

    byte[] bytes();

    default String base64String() {
        return Base64.encode(bytes());
    }

    static Script fromBytes(byte[] bytes, boolean isAsset) {
        return JavaAdapter.parseBytes(bytes, isAsset);
    }

    static Script fromBase64String(String base64, boolean isAsset) {
        return JavaAdapter.parseBytes(Base64.decode(base64), isAsset);
    }
}
