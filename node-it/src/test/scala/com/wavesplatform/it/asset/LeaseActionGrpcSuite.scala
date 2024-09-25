package com.wavesplatform.it.asset

import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.api.grpc.LeaseResponse
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.grpc.GrpcBaseTransactionSuite
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.traits.domain.Lease
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.protobuf.transaction.Recipient
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class LeaseActionGrpcSuite extends GrpcBaseTransactionSuite {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 2, Seq.empty)
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.SynchronousCalls.id, 1)))
      .buildNonConflicting()

  private def compile(script: String): Script =
    ScriptCompiler.compile(script, ScriptEstimatorV3.latest).explicitGet()._1

  private val dAppLeaseAmount       = 123
  private val txLeaseAmount         = 456
  private lazy val dAppAcc          = firstAcc
  private lazy val dAppAddress      = ByteString.copyFrom(firstAcc.toAddress.bytes)
  private lazy val dAppRecipient    = Recipient().withPublicKeyHash(firstAddress)
  private lazy val invoker          = secondAcc
  private lazy val invokerAddress   = ByteString.copyFrom(secondAcc.toAddress.bytes)
  private lazy val invokerRecipient = Recipient().withPublicKeyHash(secondAddress)

  test("set script") {
    val dApp = compile(
      s"""
         |  {-# STDLIB_VERSION 5 #-}
         |  {-# CONTENT_TYPE DAPP #-}
         |  {-# SCRIPT_TYPE ACCOUNT #-}
         |
         |  @Callable(i)
         |  func lease() = {
         |    [
         |      Lease(i.caller, $dAppLeaseAmount)
         |    ]
         |  }
         |
         |  @Callable(i)
         |  func leaseCancel(leaseId: ByteVector) = {
         |    [
         |      LeaseCancel(leaseId)
         |    ]
         |  }
     """.stripMargin
    )
    sender.setScript(dAppAcc, Right(Some(dApp)), waitForTx = true)
  }

  test("active leases") {
    val leaseTxId     = sender.broadcastLease(dAppAcc, invokerRecipient, txLeaseAmount, smartMinFee, TxVersion.V2, waitForTx = true).id
    val leaseTxHeight = sender.getStatus(leaseTxId).height.toInt

    val invokeId     = sender.broadcastInvokeScript(invoker, dAppRecipient, Some(FUNCTION_CALL(User("lease"), Nil)), Nil, waitForTx = true).id
    val invokeHeight = sender.getStatus(invokeId).height.toInt

    val recipient     = Address(ByteStr(invokerAddress.toByteArray))
    val leaseActionId = Lease.calculateId(Lease(recipient, dAppLeaseAmount, 0), ByteStr.decodeBase58(invokeId).get).toString

    sender.getActiveLeases(dAppAddress) should contain theSameElementsAs Seq(
      LeaseResponse(leaseTxId, leaseTxId, dAppAddress, Some(invokerRecipient), txLeaseAmount, leaseTxHeight),
      LeaseResponse(leaseActionId, invokeId, dAppAddress, Some(invokerRecipient), dAppLeaseAmount, invokeHeight)
    )

    val leaseTxIdParam = List(CONST_BYTESTR(ByteStr.decodeBase58(leaseTxId).get).explicitGet())
    sender.broadcastInvokeScript(dAppAcc, dAppRecipient, Some(FUNCTION_CALL(User("leaseCancel"), leaseTxIdParam)), waitForTx = true)
    sender.getActiveLeases(dAppAddress) shouldBe Seq(
      LeaseResponse(leaseActionId, invokeId, dAppAddress, Some(invokerRecipient), dAppLeaseAmount, invokeHeight)
    )
  }
}
