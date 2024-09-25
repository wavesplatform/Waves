package com.wavesplatform.api.http

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.ScriptResultV3
import com.wavesplatform.lang.v1.traits.domain.DataItem.Lng
import com.wavesplatform.lang.v1.traits.domain.{AssetTransfer, Recipient}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.{Proofs, Transaction, TxValidationError}
import com.wavesplatform.utils.JsonMatchers
import play.api.libs.json.{JsObject, Json}

class TraceResultJsonTest extends PropSpec with JsonMatchers {
  private val tx = (
    for {
      publicKey <- PublicKey.fromBase58String("9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT")
      address   <- Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU")
      proof = ByteStr.decodeBase58("4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ").get
      tx <- InvokeScriptTransaction.create(
        1.toByte,
        sender = publicKey,
        dappAddress = address,
        fc = Some(FUNCTION_CALL(User("func"), List(CONST_STRING("param").explicitGet(), CONST_LONG(1)))),
        p = List(InvokeScriptTransaction.Payment(1L, Waves)),
        fee = 10000000L,
        feeAssetId = Waves,
        timestamp = 1111L,
        proofs = Proofs(List(proof)),
        address.chainId
      )
    } yield tx
  ).explicitGet()

  def json[E, A](result: TracedResult[E, A])(implicit ev1: E => ApiError, ev2: A => Transaction): JsObject = {
    val resultJson = result.resultE match {
      case Right(value) => value.json()
      case Left(e)      => e.json
    }
    resultJson ++ Json.obj("trace" -> result.trace.map(_.json))
  }

  def loggedJson[E, A](result: TracedResult[E, A])(implicit ev1: E => ApiError, ev2: A => Transaction): JsObject =
    json(result) ++ Json.obj("trace" -> result.trace.map(_.loggedJson))

  property("suitable TracedResult json") {
    val vars = List(
      "amount"     -> Right(CONST_LONG(12345)),
      "invocation" -> CONST_STRING("str")
    )
    val recipient = Recipient.Address(ByteStr(tx.dApp.bytes))
    val trace = List(
      InvokeScriptTrace(
        tx.id(),
        tx.dApp,
        tx.funcCall,
        Right(
          ScriptResultV3(
            List(Lng("3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To", 700000000)),
            List(AssetTransfer(recipient, recipient, 1, None)),
            0
          )
        ),
        vars,
        Nil
      )
    )

    val result = TracedResult(Right(tx), trace)
    json(result) should matchJson("""{
                                    |  "type": 16,
                                    |  "id": "2hoMeTHAneLExjFo2a9ei7D4co5zzr9VyT7tmBmAGmeu",
                                    |  "sender": "3MvtiFpnSA7uYKXV3myLwRK3u2NEV91iJYW",
                                    |  "senderPublicKey": "9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT",
                                    |  "fee": 10000000,
                                    |  "feeAssetId": null,
                                    |  "timestamp": 1111,
                                    |  "proofs": [
                                    |    "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ"
                                    |  ],
                                    |  "version": 1,
                                    |  "dApp": "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
                                    |  "payment": [
                                    |    {
                                    |      "amount": 1,
                                    |      "assetId": null
                                    |    }
                                    |  ],
                                    |  "call": {
                                    |    "function": "func",
                                    |    "args": [
                                    |      {
                                    |        "type": "string",
                                    |        "value": "param"
                                    |      },
                                    |      {
                                    |        "type": "integer",
                                    |        "value": 1
                                    |      }
                                    |    ]
                                    |  },
                                    |  "trace": [
                                    |    {
                                    |      "type": "dApp",
                                    |      "id": "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
                                    |      "function": "func",
                                    |      "args": [
                                    |        "param",
                                    |        "1"
                                    |      ],
                                    |      "invocations": [],
                                    |      "result": {
                                    |        "data": [
                                    |          {
                                    |            "key": "3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To",
                                    |            "type": "integer",
                                    |            "value": 700000000
                                    |          }
                                    |        ],
                                    |        "transfers": [
                                    |          {
                                    |            "address": "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
                                    |            "asset": null,
                                    |            "amount": 1
                                    |          }
                                    |        ],
                                    |        "issues": [],
                                    |        "reissues": [],
                                    |        "burns": [],
                                    |        "sponsorFees": [],
                                    |        "leases" : [],
                                    |        "leaseCancels" : [],
                                    |        "invokes": []
                                    |      },
                                    |      "error": null
                                    |    }
                                    |  ]
                                    |}""".stripMargin)

    loggedJson(result) should matchJson(
      """{
        |  "type": 16,
        |  "id": "2hoMeTHAneLExjFo2a9ei7D4co5zzr9VyT7tmBmAGmeu",
        |  "sender": "3MvtiFpnSA7uYKXV3myLwRK3u2NEV91iJYW",
        |  "senderPublicKey": "9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT",
        |  "fee": 10000000,
        |  "feeAssetId": null,
        |  "timestamp": 1111,
        |  "proofs": [
        |    "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ"
        |  ],
        |  "version": 1,
        |  "dApp": "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |  "payment": [
        |    {
        |      "amount": 1,
        |      "assetId": null
        |    }
        |  ],
        |  "call": {
        |    "function": "func",
        |    "args": [
        |      {
        |        "type": "string",
        |        "value": "param"
        |      },
        |      {
        |        "type": "integer",
        |        "value": 1
        |      }
        |    ]
        |  },
        |  "trace": [
        |    {
        |      "type": "dApp",
        |      "id": "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |      "function": "func",
        |      "args": [
        |        "param",
        |        "1"
        |      ],
        |      "invocations": [],
        |      "result": {
        |        "data": [
        |          {
        |            "key": "3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To",
        |            "type": "integer",
        |            "value": 700000000
        |          }
        |        ],
        |        "transfers": [
        |          {
        |            "address": "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
        |            "asset": null,
        |            "amount": 1
        |          }
        |        ],
        |        "issues": [],
        |        "reissues": [],
        |        "burns": [],
        |        "sponsorFees": [],
        |        "leases" : [],
        |        "leaseCancels" : [],
        |        "invokes": []
        |      },
        |      "error": null,
        |      "vars": [
        |        {
        |          "name": "amount",
        |          "type": "Int",
        |          "value": 12345
        |        },
        |        {
        |          "name": "invocation",
        |          "type": "String",
        |          "value": "str"
        |        }
        |      ]
        |    }
        |  ]
        |}""".stripMargin
    )
  }

  property("suitable TracedResult error json") {
    val vars = List(
      "amount"     -> Right(CONST_LONG(12345)),
      "invocation" -> CONST_STRING("str")
    )
    val reason = "error reason"

    val trace = List(
      InvokeScriptTrace(
        tx.id(),
        tx.dApp,
        tx.funcCall,
        Left(TxValidationError.ScriptExecutionError(reason, vars, None)),
        vars,
        Nil
      )
    )
    val scriptExecutionError = ScriptExecutionError(tx, reason, isTokenScript = false)

    val result = TracedResult(Left(scriptExecutionError), trace)
    json(result) should matchJson("""{
                                    |  "error": 306,
                                    |  "message": "Error while executing account-script: error reason",
                                    |  "transaction": {
                                    |    "type": 16,
                                    |    "id": "2hoMeTHAneLExjFo2a9ei7D4co5zzr9VyT7tmBmAGmeu",
                                    |    "sender": "3MvtiFpnSA7uYKXV3myLwRK3u2NEV91iJYW",
                                    |    "senderPublicKey": "9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT",
                                    |    "fee": 10000000,
                                    |    "feeAssetId": null,
                                    |    "timestamp": 1111,
                                    |    "proofs": [
                                    |      "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ"
                                    |    ],
                                    |    "version": 1,
                                    |    "dApp": "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
                                    |    "payment": [
                                    |      {
                                    |        "amount": 1,
                                    |        "assetId": null
                                    |      }
                                    |    ],
                                    |    "call": {
                                    |      "function": "func",
                                    |      "args": [
                                    |        {
                                    |          "type": "string",
                                    |          "value": "param"
                                    |        },
                                    |        {
                                    |          "type": "integer",
                                    |          "value": 1
                                    |        }
                                    |      ]
                                    |    }
                                    |  },
                                    |  "trace": [
                                    |    {
                                    |      "type": "dApp",
                                    |      "id": "3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU",
                                    |      "function": "func",
                                    |      "args": [
                                    |        "param",
                                    |        "1"
                                    |      ],
                                    |      "invocations": [],
                                    |      "result": "failure",
                                    |      "vars": [
                                    |        {
                                    |          "name": "amount",
                                    |          "type": "Int",
                                    |          "value": 12345
                                    |        },
                                    |        {
                                    |          "name": "invocation",
                                    |          "type": "String",
                                    |          "value": "str"
                                    |        }
                                    |      ],
                                    |      "error": "error reason"
                                    |    }
                                    |  ]
                                    |}""".stripMargin)
  }
}
