package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V8
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers.*

class CalculateDelayTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private val contract =
    TestCompiler(V8).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   let address1 = i.caller
         |   let address2 = Address(base58'${signer(2).toAddress}')
         |   let address3 = Address(base58'${signer(3).toAddress}')
         |   let lowest   = calculateDelay(lastBlock.generationSignature, lastBlock.baseTarget, address1, 10 * 1000 * 1000)
         |   let medium   = calculateDelay(lastBlock.generationSignature, lastBlock.baseTarget, address2, 30 * 1000 * 1000)
         |   let largest  = calculateDelay(lastBlock.generationSignature, lastBlock.baseTarget, address3, 90 * 1000 * 1000)
         |   [
         |     IntegerEntry("lowest", lowest),
         |     IntegerEntry("medium", medium),
         |     IntegerEntry("largest", largest)
         |   ]
         | }
         |
         | @Callable(i)
         | func error1() = {
         |   strict r = calculateDelay(base58'${ByteStr.fill(97)(1)}', 0, i.caller, 0)
         |   []
         | }
         |
         | @Callable(i)
         | func error2() = {
         |   strict r = calculateDelay(lastBlock.generationSignature, 0, Address(base58'${ByteStr.fill(27)(1)}'), 0)
         |   []
         | }
       """.stripMargin
    )

  property("distribution of calculateDelay()") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      d.appendBlock(setScript(secondSigner, contract))
      val minDelays =
        (1 to 200).map { _ =>
          d.appendBlock(invoke())
          d.liquidDiff.accountData(secondAddress).values.minBy(_.value.asInstanceOf[Long]).key
        }
      val lowestIsMiner  = minDelays.count(_ == "lowest")
      val mediumIsMiner  = minDelays.count(_ == "medium")
      val largestIsMiner = minDelays.count(_ == "largest")
      lowestIsMiner should be > 0
      mediumIsMiner should be > lowestIsMiner
      largestIsMiner should be > mediumIsMiner
    }
  }

  property("errors of calculateDelay()") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      d.appendBlock(setScript(secondSigner, contract))
      d.appendBlockE(invoke(func = Some("error1"))) should produce("Hit source bytes length = 97 exceeds limit = 96")
      d.appendBlockE(invoke(func = Some("error2"))) should produce("Address bytes length = 27 exceeds limit = 26")
    }
  }
}
