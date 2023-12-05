package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V8
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers.*

class CalculateDelayTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private val contract =
    TestCompiler(V8).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   let hitSource = if (height % 2 == 0) then lastBlock.generationSignature else lastBlock.vrf.value()
         |   let address1  = i.caller
         |   let address2  = Address(base58'${signer(2).toAddress}')
         |   let address3  = Address(base58'${signer(3).toAddress}')
         |   let lowest    = calculateDelay(hitSource, address1, 10 * 1000 * 1000)
         |   let medium    = calculateDelay(hitSource, address2, 30 * 1000 * 1000)
         |   let largest   = calculateDelay(hitSource, address3, 90 * 1000 * 1000)
         |   [
         |     IntegerEntry("lowest", lowest),
         |     IntegerEntry("medium", medium),
         |     IntegerEntry("largest", largest)
         |   ]
         | }
         | 
         | @Callable(i)
         | func results() =
         |   [
         |     IntegerEntry("1", calculateDelay(base58'', Address(base58''), 1)),
         |     IntegerEntry("2", calculateDelay(base58'${ByteStr.fill(96)(1)}', Address(base58'${ByteStr.fill(26)(1)}'), ${Int.MaxValue})),
         |     IntegerEntry("3", calculateDelay(base58'${ByteStr.fill(96)(1)}', Address(base58'${ByteStr.fill(26)(1)}'), ${50_000L * Int.MaxValue})),
         |     IntegerEntry("4", calculateDelay(base58'${ByteStr.fill(96)(1)}', Address(base58'${ByteStr.fill(26)(1)}'), ${75_000L * Int.MaxValue}))
         |   ]
         |
         | @Callable(i)
         | func error1() = {
         |   strict r = calculateDelay(base58'${ByteStr.fill(97)(1)}', i.caller, 1)
         |   []
         | }
         |
         | @Callable(i)
         | func error2() = {
         |   strict r = calculateDelay(lastBlock.generationSignature, Address(base58'${ByteStr.fill(27)(1)}'), 1)
         |   []
         | }
         |
         | @Callable(i)
         | func error3(balance: Int) = {
         |   strict r = calculateDelay(lastBlock.generationSignature, Address(base58''), balance)
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
          d.liquidSnapshot.accountData(secondAddress).values.minBy(_.value.asInstanceOf[Long]).key
        }
      val lowestIsMiner  = minDelays.count(_ == "lowest")
      val mediumIsMiner  = minDelays.count(_ == "medium")
      val largestIsMiner = minDelays.count(_ == "largest")
      lowestIsMiner should be > 0
      mediumIsMiner should be > lowestIsMiner
      largestIsMiner should be > mediumIsMiner
    }
  }

  property("results") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      d.appendBlock(setScript(secondSigner, contract))
      d.appendBlock(invoke(func = Some("results")))
      d.liquidSnapshot.accountData.head._2.values.toSeq shouldBe Seq(
        IntegerDataEntry("1", 1612717),
        IntegerDataEntry("2", 44183),
        IntegerDataEntry("3", 1),
        IntegerDataEntry("4", 0)
      )
    }
  }

  property("errors of calculateDelay()") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      d.appendBlock(setScript(secondSigner, contract))
      d.appendBlockE(invoke(func = Some("error1"))) should produce("Hit source bytes length = 97 exceeds limit = 96")
      d.appendBlockE(invoke(func = Some("error2"))) should produce("Address bytes length = 27 exceeds limit = 26")
      d.appendBlockE(invoke(func = Some("error3"), args = Seq(CONST_LONG(-1)))) should produce("Unexpected non-positive balance = -1")
      d.appendBlockE(invoke(func = Some("error3"), args = Seq(CONST_LONG(0)))) should produce("Unexpected non-positive balance = 0")
    }
  }
}
