package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EXPR}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.SetScriptTransaction

class SyncDAppTxFailOrRejectTest extends PropSpec with WithDomain {

  val invoker: KeyPair = TxHelpers.signer(1)
  val dApp1: KeyPair   = TxHelpers.signer(2)
  val dApp2: KeyPair   = TxHelpers.signer(3)
  val dApp3: KeyPair   = TxHelpers.signer(4)
  val dApp4: KeyPair   = TxHelpers.signer(5)

  val balances: Seq[AddrWithBalance] =
    Seq(invoker, dApp1, dApp2, dApp3, dApp4).map(acc => AddrWithBalance(acc.toAddress, 10.waves))

  val settings: WavesSettings = DomainPresets.RideV5
    .configure(_.copy(enforceTransferValidationAfter = 0))
    .setFeaturesHeight(BlockchainFeatures.RideV6 -> 4)

  property(
    "reject before RideV6 (dApp1 complexity = 797, dApp2 complexity = 130) and fail after RideV6 (total complexity = 1105)"
  ) {
    createTestCase("case1", failBeforeRideV6 = false, failAfterRideV6 = true)
  }

  property(
    "reject before RideV6 (dApp1 complexity = 797, dApp2 complexity = 214) and reject after RideV6 (total complexity = 985)"
  ) {
    createTestCase("case2", failBeforeRideV6 = false, failAfterRideV6 = false)
  }

  property(
    "reject before RideV6 (dApp1 complexity = 797, dApp2 complexity = 487) and fail after RideV6 (total complexity = 1258)"
  ) {
    createTestCase("case3", failBeforeRideV6 = false, failAfterRideV6 = true)
  }

  property(
    "reject before RideV6 (dApp1 complexity = 1516) and fail after RideV6 (total complexity = 1884)"
  ) {
    createTestCase("case4", failBeforeRideV6 = false, failAfterRideV6 = true)
  }

  property(
    "reject before RideV6 (dApp1 complexity = 765, dApp2 complexity = 181, dApp3 complexity = 191) and fail after RideV6 (total complexity = 1101)"
  ) {
    createTestCase("case5", failBeforeRideV6 = false, failAfterRideV6 = true)
  }

  property(
    "fail before RideV6 (dApp1 complexity = 765, dApp2 complexity = 259, dApp3 complexity = 191) and fail after RideV6 (total complexity = 1179)"
  ) {
    createTestCase("case6", failBeforeRideV6 = true, failAfterRideV6 = true)
  }

  property(
    "self-invoke script: reject before RideV6 (total complexity without last two invokes = 904) and reject after RideV6 (total complexity = 958)"
  ) {
    createTestCase(
      "call",
      setScriptTxs = Seq(createSetSelfInvokeScriptTx),
      args = Seq(CONST_LONG(9)),
      failBeforeRideV6 = false,
      failAfterRideV6 = false
    )
  }

  property(
    "self-invoke script: fail before RideV6 (total complexity without last two invokes = 1017) and fail after RideV6 (total complexity = 1064)"
  ) {
    createTestCase(
      "call",
      setScriptTxs = Seq(createSetSelfInvokeScriptTx),
      args = Seq(CONST_LONG(10)),
      failBeforeRideV6 = true,
      failAfterRideV6 = true
    )
  }

  private def createTestCase(
      funcName: String,
      failBeforeRideV6: Boolean,
      failAfterRideV6: Boolean,
      args: Seq[EXPR] = Seq.empty,
      setScriptTxs: Seq[SetScriptTransaction] = createSetSimpleScriptsTxs
  ) =
    withDomain(settings, balances) { d =>
      val invoke = () => TxHelpers.invoke(dApp1.toAddress, func = Some(funcName), args = args, invoker = invoker)

      d.appendBlock(setScriptTxs*)

      if (failBeforeRideV6) {
        d.appendAndAssertFailed(invoke())
      } else {
        d.appendAndCatchError(invoke())
        d.appendBlock()
      }

      // RIDE V6 activation height
      if (failAfterRideV6) {
        d.appendAndAssertFailed(invoke())
      } else {
        d.appendAndCatchError(invoke())
      }
    }

  private def createSetSimpleScriptsTxs: Seq[SetScriptTransaction] = {
    val setScript1 = TxHelpers.setScript(dApp1, dAppContract1(dApp2.toAddress))
    val setScript2 = TxHelpers.setScript(dApp2, dAppContract2(dApp3.toAddress))
    val setScript3 = TxHelpers.setScript(dApp3, dAppContract3(dApp4.toAddress))
    val setScript4 = TxHelpers.setScript(dApp4, dAppContract4)

    Seq(setScript1, setScript2, setScript3, setScript4)
  }

  private def createSetSelfInvokeScriptTx: SetScriptTransaction =
    TxHelpers.setScript(dApp1, selfInvokeContract(invoker.toAddress))

  private def dAppContract1(dApp2: Address): Script =
    TestCompiler(V5).compileContract(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |let dapp2 = Address(base58'${dApp2.toString}')
         |  
         |let message = base58'emsY'
         |let pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |let sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |
         |let complex = sigVerify(message, sig, pub) && sigVerify(message, sig, pub) && sigVerify(message, sig, pub) && sigVerify_64Kb(message, sig, pub) 
         |let complexCase4 = sigVerify(message, sig, pub) && sigVerify(message, sig, pub) && sigVerify(message, sig, pub) && sigVerify_64Kb(message, sig, pub) 
         |let complexCase56 = sigVerify(message, sig, pub) && sigVerify(message, sig, pub) && sigVerify(message, sig, pub) && sigVerify_32Kb(message, sig, pub) 
         |
         |@Callable(i)
         |func case1() = {
         |  strict inv = invoke(dapp2, "case1", [complex], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case2() = {
         |  strict inv = invoke(dapp2, "case2", [complex], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case3() = {
         |  strict inv = invoke(dapp2, "case3", [complex], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case4() = {
         |  strict inv = invoke(dapp2, "case4", [complex && complexCase4], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case5() = {
         |  strict inv = invoke(dapp2, "case5", [complexCase56], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case6() = {
         |  strict inv = invoke(dapp2, "case6", [complexCase56], [])
         |  []
         |}
         |""".stripMargin
    )

  private def dAppContract2(dApp3: Address): Script =
    TestCompiler(V5).compileContract(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |let dapp3 = Address(base58'${dApp3.toString}')
         |  
         |let message = base58'emsY'
         |let pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |let sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |
         |let complexCase1 = sigVerify_8Kb(message, sig, pub) 
         |let complexCase2 = sigVerify_32Kb(message, sig, pub) && sigVerify_16Kb(message, sig, pub)
         |let complexCase34 = sigVerify(message, sig, pub) && sigVerify(message, sig, pub) 
         |let complexCase5 = sigVerify_8Kb(message, sig, pub) && sigVerify_8Kb(message, sig, pub)
         |let complexCase6 = sigVerify_64Kb(message, sig, pub) && sigVerify_32Kb(message, sig, pub)
         |
         |@Callable(i)
         |func case1(bool:Boolean) = {
         |  strict inv = invoke(dapp3, "case1", [complexCase1], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case2(bool:Boolean) = {
         |  strict inv = invoke(dapp3, "case2", [complexCase2], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case3(bool:Boolean) = {
         |  strict inv = invoke(dapp3, "case3", [complexCase34], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case4(bool:Boolean) = {
         |  if (complexCase34) then {
         |    [ScriptTransfer(dapp3, 99900000000, unit)]
         |   } else []
         |}
         |
         |@Callable(i)
         |func case5(bool:Boolean) = {
         |  strict inv = invoke(dapp3, "case5", [complexCase5], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case6(bool:Boolean) = {
         |  strict inv = invoke(dapp3, "case6", [complexCase6], [])
         |  []
         |}
         |""".stripMargin
    )

  private def dAppContract3(dApp4: Address): Script =
    TestCompiler(V5).compileContract(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |  
         |let message = base58'emsY'
         |let pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |let sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |
         |let dapp4 = Address(base58'${dApp4.toString}')
         |
         |let complexCase56 = sigVerify_16Kb(message, sig, pub) && sigVerify_8Kb(message, sig, pub)
         |
         |@Callable(i)
         |func case1(bool:Boolean) = {
         |  if (sigVerify(message, sig, pub)) then
         |    [ScriptTransfer(i.caller, 99900000000, unit)]
         |  else []
         |}
         |
         |@Callable(i)
         |func case2(bool:Boolean) = {
         |  [ScriptTransfer(i.caller, 99900000000, unit)]
         |}
         |
         |@Callable(i)
         |func case3(bool:Boolean) = {
         |  [ScriptTransfer(i.caller, 99900000000, unit)]
         |}
         |
         |@Callable(i)
         |func case5(bool:Boolean) = {
         |  strict inv = invoke(dapp4, "case5", [complexCase56], [])
         |  []
         |}
         |
         |@Callable(i)
         |func case6(bool:Boolean) = {
         |  strict inv = invoke(dapp4, "case6", [complexCase56], [])
         |  []
         |}
         |""".stripMargin
    )

  private def dAppContract4: Script =
    TestCompiler(V5).compileContract(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable(i)
         |func case5(bool:Boolean) = {
         |  [ScriptTransfer(i.caller, 99900000000, unit)]
         |}
         |
         |@Callable(i)
         |func case6(bool:Boolean) = {
         |  [ScriptTransfer(i.caller, 99900000000, unit)]
         |}
         |        
         |""".stripMargin
    )

  private def selfInvokeContract(recipient: Address): Script =
    TestCompiler(V5).compileContract(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable(i)
         |func call( r: Int ) = {
         |  if( r == 0 ) then [ ScriptTransfer(Address(base58'${recipient.toString}'), 1000000000, unit ) ] else
         |  let f = fraction( fraction( r, 1, 1 ), 1, 1 )
         |  strict g = invoke( this, "call", [ f - 1 ], [] )
         |  []
         |}  
         |""".stripMargin
    )
}
