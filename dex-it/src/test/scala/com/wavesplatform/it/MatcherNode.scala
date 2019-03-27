package com.wavesplatform.it

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.{BeforeAndAfterAll, Suite}

trait MatcherNode extends BeforeAndAfterAll with Nodes with ScorexLogging {
  this: Suite =>

  def setContract(contractText: Option[String], acc: PrivateKeyAccount): String = {
    val script = contractText.map { x =>
      val scriptText = x.stripMargin
      ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
    }
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(acc, script, 0.014.waves, System.currentTimeMillis())
      .explicitGet()

    nodes.head
      .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
      .id
  }
}
