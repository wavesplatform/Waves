package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.util.DoubleExt
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.{NTPTime, NoShrink, TransactionGen}
import monix.reactive.Observer
import org.scalatest.FreeSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InvokeScriptComplexitySpec extends FreeSpec with WithDomain with ScalaCheckPropertyChecks with TransactionGen with NTPTime with NoShrink {
  private[this] val dApp1 = TestCompiler(V5).compileContract("""
      |{-# STDLIB_VERSION 5 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}@Callable(i)
      |func call(k: String, childAddress: ByteVector, assetId: ByteVector) = {
      |    let key = takeRight(toBase58String(keccak256_16Kb((k+"x").toBytes())), 15)
      |    let payment = AttachedPayment(assetId, 1)
      |    strict z = invoke(Address(childAddress), "call", [key, assetId],[payment])
      |    let val1 = match (z) {
      |        case t:String => takeRight(toBase58String(sha256_16Kb(("x" + t).toBytes())), 50)
      |        case _ => "2"
      |      }
      |    ([StringEntry(key, val1)], val1)
      |}
      |""".stripMargin)

  private[this] val dApp0 = TestCompiler(V5).compileContract("""
      |{-# STDLIB_VERSION 5 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}@Callable(i)
      |func call(k: String, assetId:ByteVector) = {
      |
      |    ([
      |      StringEntry(k, k),
      |      BinaryEntry("asset", assetId)
      |    ], k
      |    )
      |}
      |""".stripMargin)

  private[this] val (smartAssetScript, smartAssetScriptComplexity) =
    ScriptCompiler(
      s"""{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |
      |let message = base58'emsY'
      |let pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
      |let sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
      |
      |${Seq.fill(16)("sigVerify(message, sig, pub)").mkString(" && ")}
      |""".stripMargin,
      isAssetScript = true,
      ScriptEstimatorV3
    ).explicitGet()

  private[this] val settings = domainSettingsWithFS(
    SettingsFromDefaultConfig.blockchainSettings.functionalitySettings.copy(preActivatedFeatures = BlockchainFeatures.implemented.map {
      case v @ BlockchainFeatures.FeeSponsorship.id =>
        v -> -SettingsFromDefaultConfig.blockchainSettings.functionalitySettings.featureCheckBlocksPeriod
      case f => f -> 0
    }.toMap)
  )

  private[this] val gen = for {
    invoker <- accountGen
    dapp0   <- accountGen
    dapp1   <- accountGen
  } yield (invoker, dapp0, dapp1)

  "correctly estimates complexity when child dApp invocation involves payment in smart asset" in forAll(gen) {
    case (invoker, dApp0KP, dApp1KP) =>
      withDomain(settings) { d =>
        val utx = new UtxPoolImpl(ntpTime, d.blockchain, Observer.stopped, settings.utxSettings)

        d.appendBlock(
          Seq(invoker.toAddress, dApp0KP.toAddress, dApp1KP.toAddress)
            .map(addr => GenesisTransaction.create(addr, 10000.waves, ntpTime.getTimestamp()).explicitGet()): _*
        )
        val issueTx = IssueTransaction
          .selfSigned(TxVersion.V3, dApp1KP, "DAPP1", "", 1000_00, 2, false, Some(smartAssetScript), 1.waves, ntpTime.getTimestamp())
          .explicitGet()

        d.appendBlock(
          SetScriptTransaction.selfSigned(TxVersion.V2, dApp0KP, Some(dApp0), 0.01.waves, ntpTime.getTimestamp()).explicitGet(),
          SetScriptTransaction.selfSigned(TxVersion.V2, dApp1KP, Some(dApp1), 0.01.waves, ntpTime.getTimestamp()).explicitGet(),
          issueTx
        )

        val invocation = InvokeScriptTransaction
          .selfSigned(
            TxVersion.V2,
            invoker,
            dApp1KP.toAddress,
            Some(
              FUNCTION_CALL(
                FunctionHeader.User("call"),
                List(
                  CONST_STRING("aaa").explicitGet(),
                  CONST_BYTESTR(ByteStr(dApp0KP.toAddress.bytes)).explicitGet(),
                  CONST_BYTESTR(issueTx.id()).explicitGet()
                )
              )
            ),
            Seq.empty,
            0.005.waves,
            Waves,
            ntpTime.getTimestamp()
          )
          .explicitGet()

        utx.putIfNew(invocation, true).resultE.explicitGet() shouldBe true
        utx.size shouldBe 1

        utx.cleanUnconfirmed()

        utx.size shouldBe 1

        utx.close()
      }
  }
}
