package com.wavesplatform.lang;

import com.wavesplatform.lang.directives.values.*;

public enum RideVersion {
    V1(V1$.MODULE$),
    V2(V2$.MODULE$),
    V3(V3$.MODULE$),
    V4(V4$.MODULE$);

    final StdLibVersion internal;

    RideVersion(StdLibVersion internal) {
        this.internal = internal;
    }
}
