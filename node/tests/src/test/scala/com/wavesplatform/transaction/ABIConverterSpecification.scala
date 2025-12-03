package com.wavesplatform.transaction

import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.PropSpec
import com.wavesplatform.utils.JsonMatchers

class ABIConverterSpecification extends PropSpec with JsonMatchers {

  property("callables with union args should be ignored") {
    val dApp =
      """
        |{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(i)
        |func withUnion(a: Int|String) = []
        |
        |@Callable(i)
        |func withListUnion(a: List[Int|String]) = []
        |
        |@Callable(i)
        |func allowedArgs(
        |  a: Boolean,
        |  b: ByteVector,
        |  c: Int,
        |  d: String,
        |  e: List[Boolean],
        |  f: List[ByteVector],
        |  g: List[Int],
        |  h: List[String]
        |) = []
        |""".stripMargin
    val script = TestCompiler(V5).compileContract(dApp)
    EthABIConverter(script).jsonABI should matchJson("""
                                                       |[
                                                       |  {
                                                       |    "name": "allowedArgs",
                                                       |    "type": "function",
                                                       |    "constant": false,
                                                       |    "payable": false,
                                                       |    "stateMutability": "nonpayable",
                                                       |    "inputs": [
                                                       |      {
                                                       |        "name": "a",
                                                       |        "type": "bool"
                                                       |      }, {
                                                       |        "name": "b",
                                                       |        "type": "bytes"
                                                       |      }, {
                                                       |        "name": "c",
                                                       |        "type": "int64"
                                                       |      }, {
                                                       |        "name": "d",
                                                       |        "type": "string"
                                                       |      }, {
                                                       |        "name": "e",
                                                       |        "type": "bool[]"
                                                       |      }, {
                                                       |        "name": "f",
                                                       |        "type": "bytes[]"
                                                       |      }, {
                                                       |        "name": "g",
                                                       |        "type": "int64[]"
                                                       |      }, {
                                                       |        "name": "h",
                                                       |        "type": "string[]"
                                                       |      }, {
                                                       |        "name": "payments",
                                                       |        "type": "tuple[]",
                                                       |        "components": [
                                                       |          {
                                                       |            "name": "assetId",
                                                       |            "type":"bytes32"
                                                       |          }, {
                                                       |            "name": "amount",
                                                       |            "type": "int64"
                                                       |          }
                                                       |        ]
                                                       |      }
                                                       |    ],
                                                       |    "outputs": []
                                                       |  }
                                                       |]
                                                       |""".stripMargin)
  }
}
