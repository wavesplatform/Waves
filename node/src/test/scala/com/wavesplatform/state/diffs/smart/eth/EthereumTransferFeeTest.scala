package com.wavesplatform.state.diffs.smart.eth

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{Asset, EthTxGenerator}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TransactionType.Transfer
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.utils.EthHelpers

class EthereumTransferFeeTest extends PropSpec with WithDomain with EthHelpers {
  import DomainPresets.*

  private val transferFee      = FeeConstants(Transfer) * FeeUnit
  private val transferSmartFee = transferFee + ScriptExtraFee

  property("smart asset should require additional fee") {
    val assetScript = TestCompiler(V5).compileExpression("true")
    val issueTx     = issue(script = Some(assetScript))
    val asset       = IssuedAsset(issueTx.id())
    val preTransfer = transfer(to = defaultSigner.toEthWavesAddress, asset = asset)
    withDomain(RideV6, Seq(AddrWithBalance(defaultSigner.toEthWavesAddress))) { d =>
      d.appendBlock(issueTx, preTransfer)
      assertMinFee(d, asset, transferSmartFee)
    }
  }

  property("non-smart asset should require standard fee") {
    val issueTx     = issue()
    val asset       = IssuedAsset(issueTx.id())
    val preTransfer = transfer(to = defaultSigner.toEthWavesAddress, asset = asset)
    withDomain(RideV6, Seq(AddrWithBalance(defaultSigner.toEthWavesAddress))) { d =>
      d.appendBlock(issueTx, preTransfer)
      assertMinFee(d, asset, transferFee)
    }
  }

  property("Waves should require standard fee") {
    withDomain(RideV6, Seq(AddrWithBalance(defaultSigner.toEthWavesAddress))) { d =>
      assertMinFee(d, Waves, transferFee)
    }
  }

  private def assertMinFee(d: Domain, asset: Asset, fee: Long) = {
    val notEnoughFeeTx = EthTxGenerator.generateEthTransfer(defaultSigner.toEthKeyPair, secondAddress, 1, asset, fee = fee - 1)
    val enoughFeeTx    = EthTxGenerator.generateEthTransfer(defaultSigner.toEthKeyPair, secondAddress, 1, asset, fee = fee)
    d.appendBlockE(notEnoughFeeTx) should produce(s"does not exceed minimal value of $fee WAVES")
    d.appendAndAssertSucceed(enoughFeeTx)
  }
}
