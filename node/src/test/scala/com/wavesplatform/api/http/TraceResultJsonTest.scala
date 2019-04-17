package com.wavesplatform.api.http

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.ScriptResult
import com.wavesplatform.lang.v1.traits.domain.DataItem.Lng
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.Json

class TraceResultJsonTest extends PropSpec with Matchers {
  property("suitable TraceResult json") {
    val invokeScriptTx = for {
      publicKey <- PublicKey.fromBase58String("9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT")
      address   <- Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU")
      proof     <- ByteStr.decodeBase58("4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ").toEither
      tx        <- InvokeScriptTransaction.create(
        sender      = publicKey,
        dappAddress = address,
        fc          = FUNCTION_CALL(User("func"), List(CONST_STRING("param"), CONST_LONG(1))),
        p           = List(Payment(1, Waves)),
        fee         = 10000000,
        feeAssetId  = Waves,
        timestamp   = 1111,
        proofs      = Proofs(List(proof))
      )
    } yield tx

    val trace = invokeScriptTx.map(tx => List(InvokeScriptTrace(
      tx.dappAddress,
      tx.fc,
      Right(ScriptResult(
        List(Lng("3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To",700000000)),
        List((Recipient.Address(tx.dappAddress.bytes), 1, None))
      ))
    ))).explicitGet()

    val value = TracedResult(
      invokeScriptTx.asInstanceOf[Either[ApiError, InvokeScriptTransaction]],
      trace
    )

    Json.prettyPrint(value.json) shouldBe
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
        |    "dAppAddress" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |    "function" : "func",
        |    "args" : [ "param", "1" ],
        |    "result" : {
        |      "dataItems" : [ {
        |        "key" : "3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To",
        |        "value" : "700000000"
        |      } ],
        |      "transaction" : [ {
        |        "address" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |        "amount" : 1,
        |        "asset" : "Waves"
        |      } ]
        |    }
        |  } ],
        |  "sender" : "3MvtiFpnSA7uYKXV3myLwRK3u2NEV91iJYW",
        |  "feeAssetId" : null,
        |  "proofs" : [ "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ" ],
        |  "dappAddress" : "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |  "payment" : [ {
        |    "amount" : 1,
        |    "assetId" : null
        |  } ],
        |  "id" : "8xJAPeQ7Xd93EXeq1bmkKD84DE2S4pyUqpeuzrQ89B2y",
        |  "timestamp" : 1111
        |}""".stripMargin
  }
}
