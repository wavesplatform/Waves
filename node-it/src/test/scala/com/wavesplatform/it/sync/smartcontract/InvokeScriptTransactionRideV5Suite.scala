package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.RideV4ActivationSuite._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import org.scalatest.CancelAfterFailure

class InvokeScriptTransactionRideV5Suite extends BaseTransactionSuite with CancelAfterFailure {

  private lazy val dAppV3PK    = sender.createKeyPair()
  private lazy val dAppV4PK    = sender.createKeyPair()
  private lazy val dAppV5PK    = sender.createKeyPair()
  private lazy val callerPK    = firstKeyPair
  private lazy val dAppV3      = dAppV3PK.toAddress.toString
  private lazy val dAppV4      = dAppV4PK.toAddress.toString
  private lazy val dAppV5      = dAppV5PK.toAddress.toString
  private lazy val dAppAliasV3 = "dapp.v3"
  private lazy val dAppAliasV4 = "dapp.v4"
  private lazy val dAppAliasV5 = "dapp.v5"

  private def alias(name: String): String = s"alias:I:$name"

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(NodeConfigs.Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.Ride4DApps.id, 0),
          (BlockchainFeatures.BlockV5.id, 0),
          (BlockchainFeatures.SynchronousCalls.id, 0)
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    val scriptV3 =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(inv)
        |func default() = WriteSet([])
        |""".stripMargin

    val scriptV4 =
      """
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(inv)
        |func default() = nil
        |""".stripMargin

    val scriptV5 =
      """
        |{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(inv)
        |func default() = nil
        |""".stripMargin

    sender.massTransfer(
      callerPK,
      List(
        Transfer(dAppV3, 10.waves),
        Transfer(dAppV4, 10.waves),
        Transfer(dAppV5, 10.waves)
      ),
      1.waves,
      waitForTx = true
    )

    sender.createAlias(dAppV3PK, dAppAliasV3, fee = 1.waves)
    sender.createAlias(dAppV4PK, dAppAliasV4, fee = 1.waves)
    sender.createAlias(dAppV5PK, dAppAliasV5, fee = 1.waves)

    sender.setScript(dAppV3PK, Some(scriptV3.compiled), setScriptFee + 100)
    sender.setScript(dAppV4PK, Some(scriptV4.compiled), setScriptFee + 10)
    sender.setScript(dAppV5PK, Some(scriptV5.compiled), setScriptFee, waitForTx = true)
  }

  // TODO enable in SC-695
  ignore("Can't invoke Ride V5 DApp via InvokeScriptTx V1") {
    assertApiError(
      sender.invokeScript(callerPK, dAppV5, version = TxVersion.V1)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed" // TODO detailed message
    }

    assertApiError(
      sender.invokeScript(callerPK, alias(dAppAliasV5), version = TxVersion.V1)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed" // TODO detailed message
    }
  }

  // TODO enable in SC-695
  ignore("Can't invoke Ride V5 DApp via InvokeScriptTx V2") {
    assertApiError(
      sender.invokeScript(callerPK, dAppV5, version = TxVersion.V2)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed" // TODO detailed message
    }

    assertApiError(
      sender.invokeScript(callerPK, alias(dAppAliasV5), version = TxVersion.V2)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed" // TODO detailed message
    }
  }

  // TODO enable in SC-695
  ignore("Can invoke Ride V5 DApp via InvokeScriptTx V3") {
    sender.invokeScript(callerPK, dAppV5, version = TxVersion.V3, waitForTx = true)
    sender.invokeScript(callerPK, alias(dAppAliasV5), version = TxVersion.V3, waitForTx = true)
  }

  // TODO enable in SC-695
  ignore("Can't invoke Ride V3 DApp via InvokeScriptTx V3 if extraFeePerStep is specified") {
    // TODO add extraFeePerStep
    assertApiError(
      sender.invokeScript(callerPK, dAppV3, version = TxVersion.V3)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed" // TODO detailed message
    }

    assertApiError(
      sender.invokeScript(callerPK, alias(dAppAliasV3), version = TxVersion.V3)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed" // TODO detailed message
    }
  }

  // TODO enable in SC-695
  ignore("Can't invoke Ride V4 DApp via InvokeScriptTx V3 if extraFeePerStep is specified") {
    // TODO add extraFeePerStep
    assertApiError(
      sender.invokeScript(callerPK, dAppV4, version = TxVersion.V3)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed" // TODO detailed message
    }

    assertApiError(
      sender.invokeScript(callerPK, alias(dAppAliasV4), version = TxVersion.V3)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed" // TODO detailed message
    }
  }

}
