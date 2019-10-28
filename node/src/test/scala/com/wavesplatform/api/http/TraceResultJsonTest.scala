package com.wavesplatform.api.http

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.ScriptResult
import com.wavesplatform.lang.v1.traits.domain.DataItem.Lng
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.{Proofs, TxValidationError}
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.Json

class TraceResultJsonTest extends PropSpec with Matchers {
  private val tx = (
    for {
      publicKey <- PublicKey.fromBase58String("9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT")
      address   <- Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU")
      proof     <- ByteStr.decodeBase58("4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ").toEither
      tx <- InvokeScriptTransaction.create(
        sender = publicKey,
        dappAddress = address,
        fc = Some(FUNCTION_CALL(User("func"), List(CONST_STRING("param").explicitGet(), CONST_LONG(1)))),
        p = List(Payment(1, Waves)),
        fee = 10000000,
        feeAssetId = Waves,
        timestamp = 1111,
        proofs = Proofs(List(proof))
      )
    } yield tx
  ).explicitGet()

  property("suitable TracedResult json") {
    val vars = List(
      "amount"     -> Right(CONST_LONG(12345)),
      "invocation" -> CONST_STRING("str")
    )
    val trace = List(
      InvokeScriptTrace(
        tx.dAppAddressOrAlias,
        tx.funcCall,
        Right(
          ScriptResult(
            List(Lng("3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To", 700000000)),
            List((Recipient.Address(tx.dAppAddressOrAlias.bytes), 1, None))
          )),
        vars
      ))

    val result = TracedResult(Right(tx), trace)

    Json.prettyPrint(result.json) shouldBe
      """{
        |  "senderPublicKey" : "9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT",
        |  "fee" : 10000000,
        |  "type" : 16,
        |  "version" : 1,
        |  "call" : {
        |    "function" : "func",
        |    "args" : [ {
        |      "type" : "string",
        |      "value" : "param"
        |    }, {
        |      "type" : "integer",
        |      "value" : 1
        |    } ]
        |  },
        |  "trace" : [ {
        |    "dApp" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |    "function" : "func",
        |    "args" : [ "param", "1" ],
        |    "result" : {
        |      "data" : [ {
        |        "key" : "3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To",
        |        "value" : "700000000"
        |      } ],
        |      "transfers" : [ {
        |        "address" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |        "amount" : 1,
        |        "assetId" : null
        |      } ]
        |    }
        |  } ],
        |  "dApp" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |  "sender" : "3MvtiFpnSA7uYKXV3myLwRK3u2NEV91iJYW",
        |  "feeAssetId" : null,
        |  "proofs" : [ "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ" ],
        |  "payment" : [ {
        |    "amount" : 1,
        |    "assetId" : null
        |  } ],
        |  "id" : "2hoMeTHAneLExjFo2a9ei7D4co5zzr9VyT7tmBmAGmeu",
        |  "timestamp" : 1111
        |}""".stripMargin

   Json.prettyPrint(result.loggedJson) shouldBe
      """{
        |  "senderPublicKey" : "9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT",
        |  "fee" : 10000000,
        |  "type" : 16,
        |  "version" : 1,
        |  "call" : {
        |    "function" : "func",
        |    "args" : [ {
        |      "type" : "string",
        |      "value" : "param"
        |    }, {
        |      "type" : "integer",
        |      "value" : 1
        |    } ]
        |  },
        |  "trace" : [ {
        |    "dApp" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |    "function" : "func",
        |    "args" : [ "param", "1" ],
        |    "result" : {
        |      "data" : [ {
        |        "key" : "3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To",
        |        "value" : "700000000"
        |      } ],
        |      "transfers" : [ {
        |        "address" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |        "amount" : 1,
        |        "assetId" : null
        |      } ],
        |      "vars" : [ {
        |        "name" : "amount",
        |        "value" : "12345"
        |      }, {
        |        "name" : "invocation",
        |        "value" : "str"
        |      } ]
        |    }
        |  } ],
        |  "dApp" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |  "sender" : "3MvtiFpnSA7uYKXV3myLwRK3u2NEV91iJYW",
        |  "feeAssetId" : null,
        |  "proofs" : [ "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ" ],
        |  "payment" : [ {
        |    "amount" : 1,
        |    "assetId" : null
        |  } ],
        |  "id" : "2hoMeTHAneLExjFo2a9ei7D4co5zzr9VyT7tmBmAGmeu",
        |  "timestamp" : 1111
        |}""".stripMargin
  }

  property("suitable TracedResult error json") {
    val vars = List(
      "amount"     -> Right(CONST_LONG(12345)),
      "invocation" -> CONST_STRING("str")
    )
    val reason = "error reason"

    val trace = List(
      InvokeScriptTrace(
        tx.dAppAddressOrAlias,
        tx.funcCall,
        Left(TxValidationError.ScriptExecutionError(reason, vars, isAssetScript = false)),
        vars
      ))
    val scriptExecutionError = ScriptExecutionError(tx, reason, isTokenScript = false)

    val result = TracedResult(Left(scriptExecutionError), trace)

    Json.prettyPrint(result.json) shouldBe
      """{
      |  "trace" : [ {
      |    "dApp" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
      |    "function" : "func",
      |    "args" : [ "param", "1" ],
      |    "error" : {
      |      "type" : "Account",
      |      "vars" : [ {
      |        "name" : "amount",
      |        "value" : "12345"
      |      }, {
      |        "name" : "invocation",
      |        "value" : "str"
      |      } ],
      |      "reason" : "error reason"
      |    }
      |  } ],
      |  "error" : 306,
      |  "message" : "Error while executing account-script: error reason",
      |  "transaction" : {
      |    "senderPublicKey" : "9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT",
      |    "call" : {
      |      "function" : "func",
      |      "args" : [ {
      |        "type" : "string",
      |        "value" : "param"
      |      }, {
      |        "type" : "integer",
      |        "value" : 1
      |      } ]
      |    },
      |    "dApp" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
      |    "sender" : "3MvtiFpnSA7uYKXV3myLwRK3u2NEV91iJYW",
      |    "feeAssetId" : null,
      |    "proofs" : [ "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ" ],
      |    "fee" : 10000000,
      |    "payment" : [ {
      |      "amount" : 1,
      |      "assetId" : null
      |    } ],
      |    "id" : "2hoMeTHAneLExjFo2a9ei7D4co5zzr9VyT7tmBmAGmeu",
      |    "type" : 16,
      |    "version" : 1,
      |    "timestamp" : 1111
      |  }
      |}""".stripMargin
  }
}
