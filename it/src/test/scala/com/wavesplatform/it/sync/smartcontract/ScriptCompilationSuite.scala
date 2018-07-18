package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.api.http.assets.{IssueV2Request, SetScriptRequest}

class ScriptCompilationSuite extends BaseTransactionSuite {
  test("Sign broadcast via rest") {
    val sender = notMiner.publicKey.address
    notMiner.signAndBroadcast(IssueV2Request(2, sender, "name", "desc", 10000, 2, false, None, 100000000, None).toJsObject)
    notMiner.signAndBroadcast(SetScriptRequest(1, sender, None, 100000, None).toJsObject)
  }
}
