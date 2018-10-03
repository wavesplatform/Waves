package com.wavesplatform.lang;

import com.wavesplatform.lang.v1.Serde;
import com.wavesplatform.lang.v1.compiler.Terms;

/**
 * This class provides static methods to interact with the RIDE compiler.
 */
public class Lang {

    /**
     * Compiles a RIDE program into an abstract syntax tree.
     *
     * @param program a RIDE program
     * @return the root of the compiled tree
     * @throws IllegalArgumentException if a compilation error occurs
     */
    public static Terms.EXPR compile(String program) {
        return JavaAdapter.compile(program);
    }

    /**
     * Serializes a compiled abstract syntax tree into a byte array.
     * This method always succeeds.
     *
     * @return the resulting byte array
     */
    public static byte[] serialize(Terms.EXPR expr) {
        return Serde.serialize(expr);
    }
}