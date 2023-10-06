package com.wavesplatform.state.diffs.smart.predef

import cats.syntax.semigroup.*
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.Testing.*
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler, TestCompiler}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.utils.*
import org.scalatest.Assertion
import shapeless.Coproduct

class ContextFunctionsTest extends PropSpec with WithDomain with EthHelpers {
  import DomainPresets.*

  private val preconditionsAndPayments = {
    val master    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val genesis = Seq(master, recipient).map(acc => TxHelpers.genesis(acc.toAddress))

    val dataTx = TxHelpers.data(
      account = recipient,
      entries = Seq(
        IntegerDataEntry("int", 1),
        BooleanDataEntry("bool", value = true),
        BinaryDataEntry("bin", ByteStr.fill(10)(1)),
        StringDataEntry("str", "test")
      )
    )
    val transfer  = TxHelpers.transfer(master, recipient.toAddress, 100000000L, version = TxVersion.V1)
    val transfer2 = TxHelpers.transfer(master, recipient.toAddress, 100000L)

    val setScripts = Seq(
      scriptWithV1PureFunctions(dataTx, transfer),
      scriptWithV1WavesFunctions(dataTx, transfer),
      scriptWithCryptoFunctions
    ).map(x => Parser.parseExpr(x).get.value)
      .map { untypedScript =>
        val typedScript = {
          val compilerScript = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untypedScript).explicitGet()._1
          ExprScript(compilerScript).explicitGet()
        }

        TxHelpers.setScript(recipient, typedScript)
      }

    (master, recipient, genesis, setScripts, dataTx, transfer, transfer2)
  }

  private val estimator = ScriptEstimatorV2

  property("validation of all functions from contexts") {
    val (_, _, genesis, setScriptTransactions, dataTransaction, transfer, _) = preconditionsAndPayments
    setScriptTransactions.foreach { setScript =>
      assertDiffAndState(smartEnabledFS) { append =>
        append(genesis).explicitGet()
        append(Seq(setScript, dataTransaction)).explicitGet()
        append(Seq(transfer)).explicitGet()
      }
    }
  }

  property("reading from data transaction array by key") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { version =>
        val (_, _, _, _, tx, _, _) = preconditionsAndPayments

        val int  = tx.data(0)
        val bool = tx.data(1)
        val bin  = tx.data(2)
        val str  = tx.data(3)
        val result = runScript(
          s"""
             |match tx {
             | case tx: DataTransaction => {
             |  let d = tx.data
             |
             |  let int  = value(getInteger(d, "${int.key}"))
             |  let bool = value(getBoolean(d, "${bool.key}"))
             |  let bin  = value(getBinary(d, "${bin.key}"))
             |  let str  = value(getString(d, "${str.key}"))
             |
             |  let intV  = getIntegerValue(d, "${int.key}")
             |  let boolV = getBooleanValue(d, "${bool.key}")
             |  let binV  = getBinaryValue(d, "${bin.key}")
             |  let strV  = getStringValue(d, "${str.key}")
             |
             |  let okInt  = int  == ${int.value}
             |  let okBool = bool == ${bool.value}
             |  let okBin  = bin  == base58'${Base58.encode(bin.asInstanceOf[BinaryDataEntry].value.arr)}'
             |  let okStr  = str  == "${str.value}"
             |
             |  let okIntV  = intV + 1  == ${int.value} + 1
             |  let okBoolV = boolV || true == ${bool.value} || true
             |  let okBinV  = binV  == base58'${Base58.encode(bin.asInstanceOf[BinaryDataEntry].value.arr)}'
             |  let okStrV  = strV + ""  == "${str.value}"
             |
             |  let badInt  = isDefined(getInteger(d, "${bool.key}"))
             |  let badBool = isDefined(getBoolean(d, "${bin.key}"))
             |  let badBin  = isDefined(getBinary(d, "${str.key}"))
             |  let badStr  = isDefined(getString(d, "${int.key}"))
             |
             |  let noSuchKey = isDefined(getInteger(d, "\u00a0"))
             |
             |  let positives = okInt && okBool && okBin && okStr && okIntV && okBoolV && okBinV && okStrV
             |  let negatives = badInt || badBool || badBin || badStr || noSuchKey
             |  positives && ! negatives
             | }
             | case _ => throw()
             |}
             |""".stripMargin,
          Coproduct(tx),
          version
        )
        result shouldBe evaluated(true)
      }
  }

  property("reading from data transaction array by index") {
    val (_, _, _, _, tx, _, _) = preconditionsAndPayments
    val badIndex               = 4

    val int  = tx.data(0)
    val bool = tx.data(1)
    val bin  = tx.data(2)
    val str  = tx.data(3)
    val ok = runScript(
      s"""
         |match tx {
         | case tx: DataTransaction => {
         |  let d = tx.data
         |
         |  let int  = extract(getInteger(d, 0))
         |  let bool = extract(getBoolean(d, 1))
         |  let bin  = extract(getBinary(d, 2))
         |  let str  = extract(getString(d, 3))
         |
         |  let okInt  = int  == ${int.value}
         |  let okBool = bool == ${bool.value}
         |  let okBin  = bin  == base58'${Base58.encode(bin.asInstanceOf[BinaryDataEntry].value.arr)}'
         |  let okStr  = str  == "${str.value}"
         |
         |  let badInt  = isDefined(getInteger(d, 1))
         |  let badBool = isDefined(getBoolean(d, 2))
         |  let badBin  = isDefined(getBinary(d, 3))
         |  let badStr  = isDefined(getString(d, 0))
         |
         |  let positives = okInt && okBool && okBin && okStr
         |  let negatives = badInt || badBool || badBin || badStr
         |  positives && ! negatives
         | }
         | case _ => throw()
         |}
         |""".stripMargin,
      Coproduct(tx)
    )
    ok shouldBe evaluated(true)

    val outOfBounds = runScript(
      s"""
         |match tx {
         | case d: DataTransaction => isDefined(getInteger(d.data, $badIndex))
         | case _ => false
         |}
         |""".stripMargin,
      Coproduct(tx)
    )
    outOfBounds shouldBe Left(s"Index $badIndex out of bounds for length ${tx.data.size}")
  }

  ignore("base64 amplification") {
    val script =
      """
        |let a = base58'7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy'
        |let b = toBase64String( a ); let c = toBytes( b )
        |let d = toBase64String( c ); let e = toBytes( d )
        |let f = toBase64String( e ); let g = toBytes( f )
        |let h = toBase64String( g ); let i = toBytes( h )
        |let j = toBase64String( i ); let k = toBytes( j )
        |let l = toBase64String( k ); let m = toBytes( l )
        |let n = toBase64String( m ); let o = toBytes( n )
        |let p = toBase64String( o ); let q = toBytes( p )
        |let r = toBase64String( q ); let s = toBytes( r )
        |let t = toBase64String( s ); let u = toBytes( t )
        |let v = toBase64String( u ); let w = toBytes( v )
        |let x = toBase64String( w ); let y = toBytes( x )
        |let z = toBase64String( y ); let a0 = toBytes( z )
        |let a1 = toBase64String( a0 ); let a2 = toBytes( a1 )
        |let a3 = toBase64String( a2 ); let a4 = toBytes( a3 )
        |let a5 = toBase64String( a4 ); let a6 = toBytes( a5 )
        |let a7 = toBase64String( a6 ); let a8 = toBytes( a7 )
        |let a9 = toBase64String( a8 ); let aa = toBytes( a9 )
        |let ab = toBase64String( aa ); let ac = toBytes( ab )
        |let ad = toBase64String( ac ); let ae = toBytes( ad )
        |let af = toBase64String( ae ); let ag = toBytes( af )
        |let ah = toBase64String( ag ); let ai = toBytes( ah )
        |let aj = toBase64String( ai ); let ak = toBytes( aj )
        |let al = toBase64String( ak ); let am = toBytes( al )
        |let an = toBase64String( am ); let ao = toBytes( an )
        |let ap = toBase64String( ao ); let aq = toBytes( ap )
        |let ar = toBase64String( aq ); let as = toBytes( ar )
        |let at = toBase64String( as ); let au = toBytes( at )
        |let av = toBase64String( au ); let aw = toBytes( av )
        |let ax = toBase64String( aw ); let ay = toBytes( ax )
        |let az = toBase64String( ay ); let b0 = toBytes( az )
        |let b1 = toBase64String( b0 ); let b2 = toBytes( b1 )
        |let b3 = toBase64String( b2 ); let b4 = toBytes( b3 )
        |let b5 = toBase64String( b4 ); let b6 = toBytes( b5 )
        |let b7 = toBase64String( b6 ); let b8 = toBytes( b7 )
        |let b9 = toBase64String( b8 ); let ba = toBytes( b9 )
        |let bb = toBase64String( ba ); let bc = toBytes( bb )
        |let bd = toBase64String( bc ); let be = toBytes( bd )
        |let bf = toBase64String( be ); let bg = toBytes( bf )
        |let bh = toBase64String( bg ); let bi = toBytes( bh )
        |let bj = toBase64String( bi ); let bk = toBytes( bj )
        |let bl = toBase64String( bk ); let bm = toBytes( bl )
        |let bn = toBase64String( bm ); let bo = toBytes( bn )
        |let bp = toBase64String( bo ); let bq = toBytes( bp )
        |let br = toBase64String( bq ); let bs = toBytes( br )
        |let bt = toBase64String( bs ); let bu = toBytes( bt )
        |let bv = toBase64String( bu ); let bw = toBytes( bv )
        |let bx = toBase64String( bw ); let by = toBytes( bx )
        |let bz = toBase64String( by ); let c0 = toBytes( bz )
        |let c1 = toBase64String( c0 ); let c2 = toBytes( c1 )
        |let c3 = toBase64String( c2 ); let c4 = toBytes( c3 )
        |let c5 = toBase64String( c4 ); let c6 = toBytes( c5 )
        |let c7 = toBase64String( c6 ); let c8 = toBytes( c7 )
        |let c9 = toBase64String( c8 ); let ca = toBytes( c9 )
        |let cb = toBase64String( ca ); let cc = toBytes( cb )
        |let cd = toBase64String( cc ); let ce = toBytes( cd )
        |let cf = toBase64String( ce ); let cg = toBytes( cf )
        |let ch = toBase64String( cg ); let ci = toBytes( ch )
        |let cj = toBase64String( ci ); let ck = toBytes( cj )
        |let cl = toBase64String( ck ); let cm = toBytes( cl )
        |let cn = toBase64String( cm ); let co = toBytes( cn )
        |let cp = toBase64String( co ); let cq = toBytes( cp )
        |let cr = toBase64String( cq ); let cs = toBytes( cr )
        |let ct = toBase64String( cs ); let cu = toBytes( ct )
        |let cv = toBase64String( cu ); let cw = toBytes( cv )
        |let cx = toBase64String( cw ); let cy = toBytes( cx )
        |let cz = toBase64String( cy ); let d0 = toBytes( cz )
        |let d1 = toBase64String( d0 ); let d2 = toBytes( d1 )
        |let d3 = toBase64String( d2 ); let d4 = toBytes( d3 )
        |let d5 = toBase64String( d4 ); let d6 = toBytes( d5 )
        |let d7 = toBase64String( d6 ); let d8 = toBytes( d7 )
        |let d9 = toBase64String( d8 ); let da = toBytes( d9 )
        |let db = toBase64String( da ); let dc = toBytes( db )
        |let dd = toBase64String( dc ); let de = toBytes( dd )
        |sha256( de ) != base58'123'
      """.stripMargin
    runScript(script) shouldBe Left(s"ByteStr size=36408 exceeds 32767 bytes")
  }

  property("get assetInfo by asset id") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { version =>
        val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, transfer2) = preconditionsAndPayments
        setScriptTransactions.foreach { setScriptTransaction =>
          val fs = settingsForRide(version).blockchainSettings.functionalitySettings

          assertDiffAndState(fs) { append =>
            append(genesis).explicitGet()
            append(Seq(setScriptTransaction, dataTransaction)).explicitGet()

            val quantity    = 100000000L
            val decimals    = 6.toByte
            val reissuable  = true
            val assetScript = None

            val issueTx = TxHelpers.issue(masterAcc, quantity, decimals)

            val sponsoredFee = 100
            val sponsorTx    = TxHelpers.sponsor(issueTx.asset, Some(sponsoredFee), masterAcc)

            append(Seq(transferTx, issueTx)).explicitGet()

            val assetId = issueTx.assetId

            val sponsored =
              if (version >= V4)
                s"let sponsored = aInfo.minSponsoredFee == $sponsoredFee"
              else
                s"let sponsored = aInfo.sponsored == true"

            val script = ScriptCompiler
              .compile(
                s"""
                   | {-# STDLIB_VERSION ${version.id} #-}
                   | {-# CONTENT_TYPE EXPRESSION #-}
                   | {-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   | let aInfoOpt        = assetInfo(base58'$assetId')
                   | let aInfo           = aInfoOpt.value()
                   | let id              = aInfo.id == base58'$assetId'
                   | let quantity        = aInfo.quantity == $quantity
                   | let decimals        = aInfo.decimals == $decimals
                   | let issuer          = aInfo.issuer.bytes == base58'${issueTx.sender.toAddress}'
                   | let issuerPublicKey = aInfo.issuerPublicKey == base58'${issueTx.sender}'
                   | let scripted        = aInfo.scripted == ${assetScript.nonEmpty}
                   | let reissuable      = aInfo.reissuable == $reissuable
                   | $sponsored
                   |
                   | id              &&
                   | quantity        &&
                   | decimals        &&
                   | issuer          &&
                   | issuerPublicKey &&
                   | scripted        &&
                   | reissuable      &&
                   | sponsored
                   |
            """.stripMargin,
                estimator
              )
              .explicitGet()
              ._1

            val setScriptTx = TxHelpers.setScript(masterAcc, script)

            append(Seq(sponsorTx)).explicitGet()
            append(Seq(setScriptTx)).explicitGet()
            append(Seq(transfer2)).explicitGet()
          }
        }
      }
  }

  property("last block info check") {
    val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, transfer2) = preconditionsAndPayments
    setScriptTransactions.foreach { setScriptTransaction =>
      assertDiffAndState(smartEnabledFS) { append =>
        append(genesis).explicitGet()
        append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
        append(Seq(transferTx)).explicitGet()

        val script = ScriptCompiler
          .compile(
            s"""
               | {-# STDLIB_VERSION 3 #-}
               | {-# CONTENT_TYPE EXPRESSION #-}
               | {-# SCRIPT_TYPE ACCOUNT #-}
               |
               | let lastBlockBaseTarget = lastBlock.baseTarget == 2
               | let lastBlockGenerationSignature = lastBlock.generationSignature == base58'${ByteStr(
              Array.fill(Block.GenerationSignatureLength)(0: Byte)
            )}'
               | let lastBlockGenerator = lastBlock.generator.bytes == base58'${TestBlock.defaultSigner.publicKey.toAddress}'
               | let lastBlockGeneratorPublicKey = lastBlock.generatorPublicKey == base58'${TestBlock.defaultSigner.publicKey}'
               |
               | lastBlockBaseTarget && lastBlockGenerationSignature && lastBlockGenerator && lastBlockGeneratorPublicKey
               |
               |
          """.stripMargin,
            estimator
          )
          .explicitGet()
          ._1

        val setScriptTx = SetScriptTransaction.selfSigned(1.toByte, masterAcc, Some(script), 1000000L, transferTx.timestamp + 5).explicitGet()

        append(Seq(setScriptTx)).explicitGet()
        append(Seq(transfer2)).explicitGet()
      }
    }
  }

  property("block info by height") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { version =>
        val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, transfer2) = preconditionsAndPayments
        for {
          setScriptTransaction <- setScriptTransactions
          withVrf              <- Seq(version >= V4, true).distinct
        } yield {
          val generationSignature =
            if (withVrf) ByteStr(new Array[Byte](Block.GenerationVRFSignatureLength)) else ByteStr(new Array[Byte](Block.GenerationSignatureLength))

          val settingsVersion = if (withVrf) Seq(version, V4).max else version
          val settings        = settingsForRide(settingsVersion).blockchainSettings.functionalitySettings

          val (v4DeclOpt, v4CheckOpt) =
            if (version >= V4)
              if (withVrf)
                (s"let checkVrf = block.vrf != unit", "&& checkVrf")
              else
                (s"let checkVrf = block.vrf == unit", "&& checkVrf")
            else ("", "")

          assertDiffAndState(settings) { append =>
            append(genesis).explicitGet()
            append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
            append(Seq(transferTx)).explicitGet()

            val script = ScriptCompiler
              .compile(
                s"""
                   | {-# STDLIB_VERSION ${version.id} #-}
                   | {-# CONTENT_TYPE EXPRESSION #-}
                   | {-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   | let nonExistedBlockNeg = !blockInfoByHeight(-1).isDefined()
                   | let nonExistedBlockZero = !blockInfoByHeight(0).isDefined()
                   | let nonExistedBlockNextPlus = !blockInfoByHeight(6).isDefined()
                   |
                   | let block = blockInfoByHeight(3).value()
                   | let checkHeight = block.height == 3
                   | let checkBaseTarget = block.baseTarget == 2
                   | let checkGenSignature = block.generationSignature == base58'$generationSignature'
                   | let checkGenerator = block.generator.bytes == base58'${TestBlock.defaultSigner.publicKey.toAddress}'
                   | let checkGeneratorPublicKey = block.generatorPublicKey == base58'${TestBlock.defaultSigner.publicKey}'
                   | $v4DeclOpt
                   |
                   | nonExistedBlockNeg && nonExistedBlockZero && nonExistedBlockNextPlus && checkHeight && checkBaseTarget && checkGenSignature && checkGenerator && checkGeneratorPublicKey
                   | $v4CheckOpt
                   |
              """.stripMargin,
                estimator
              )
              .explicitGet()
              ._1

            val setScriptTx = TxHelpers.setScript(masterAcc, script)

            append(Seq(setScriptTx)).explicitGet()
            append(Seq(transfer2)).explicitGet()
          }
        }
      }
  }

  property("blockInfoByHeight(height) is the same as lastBlock") {
    val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, _) = preconditionsAndPayments
    setScriptTransactions.foreach { setScriptTransaction =>
      assertDiffAndState(smartEnabledFS) { append =>
        append(genesis).explicitGet()
        append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
        append(Seq(transferTx)).explicitGet()

        val script =
          s"""{-# STDLIB_VERSION 3 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |
             |@Callable(xx)
             |func compareBlocks() = {
             |     let lastBlockByHeight = extract(blockInfoByHeight(height))
             |
             |     if lastBlock.height              == lastBlockByHeight.height &&
             |        lastBlock.timestamp           == lastBlockByHeight.timestamp &&
             |        lastBlock.baseTarget          == lastBlockByHeight.baseTarget &&
             |        lastBlock.generationSignature == lastBlockByHeight.generationSignature &&
             |        lastBlock.generator           == lastBlockByHeight.generator &&
             |        lastBlock.generatorPublicKey  == lastBlockByHeight.generatorPublicKey
             |     then WriteSet([])
             |     else throw("blocks do not match")
             |}
             |""".stripMargin

        val compiledScript = TestCompiler(V3).compileContract(script)
        val setScriptTx    = TxHelpers.setScript(masterAcc, compiledScript)

        val invoke = TxHelpers.invoke(
          dApp = masterAcc.toAddress,
          func = Some("compareBlocks"),
          invoker = masterAcc,
          fee = TxHelpers.ciFee(1),
          version = TxVersion.V1
        )

        append(Seq(setScriptTx)).explicitGet()
        append(Seq(invoke)).explicitGet()
      }
    }
  }

  property(
    s"blockInfoByHeight(height) and lastBlock result contains rewards field in Ride V7 after ${BlockchainFeatures.BlockRewardDistribution.description} activation"
  ) {
    val invoker           = TxHelpers.signer(1)
    val dApp              = TxHelpers.signer(2)
    val daoAddress        = TxHelpers.address(3)
    val xtnBuybackAddress = TxHelpers.address(4)

    val rideV6Settings = RideV6
      .copy(blockchainSettings =
        RideV6.blockchainSettings.copy(functionalitySettings =
          RideV6.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString))
        )
      )
      .setFeaturesHeight(BlockchainFeatures.BlockRewardDistribution -> 4)

    Seq("value(blockInfoByHeight(2)).rewards", "lastBlock.rewards").foreach { getRewardsCode =>
      Seq(V4, V5, V6).foreach(v =>
        TestCompiler(v).compile(blockInfoScript(v, getRewardsCode)) should produce("Undefined field `rewards` of variable of type `BlockInfo`")
      )

      val compiledDapp = TestCompiler(V7).compile(blockInfoScript(V7, getRewardsCode))
      compiledDapp should beRight

      withDomain(rideV6Settings, balances = AddrWithBalance.enoughBalances(invoker, dApp)) { d =>
        val invoke = () => TxHelpers.invoke(dApp.toAddress, Some("foo"), invoker = invoker)

        val miner = d.appendBlock(TxHelpers.setScript(dApp, ContractScript(V6, compiledDapp.explicitGet()).explicitGet())).sender.toAddress
        d.appendBlockE(invoke()) should produce("key not found: rewards")

        d.appendBlockE(TxHelpers.setScript(dApp, ContractScript(V7, compiledDapp.explicitGet()).explicitGet())) should produce(
          "Block Reward Distribution feature has not been activated yet"
        )

        d.appendBlock()
        d.appendBlockE(TxHelpers.setScript(dApp, ContractScript(V7, compiledDapp.explicitGet()).explicitGet())) should beRight
        d.appendBlockE(invoke()) should beRight

        val configAddressReward = d.blockchain.settings.rewardsSettings.initial / 3
        d.blockchain.accountData(dApp.toAddress, "size") shouldBe Some(IntegerDataEntry("size", 3))
        (1 to 3).map { idx =>
          (
            d.blockchain.accountData(dApp.toAddress, s"addr$idx").get.asInstanceOf[BinaryDataEntry].value,
            d.blockchain.accountData(dApp.toAddress, s"reward$idx").get.asInstanceOf[IntegerDataEntry].value
          )
        } shouldBe Seq(
          ByteStr(miner.bytes)             -> (d.blockchain.settings.rewardsSettings.initial - 2 * configAddressReward),
          ByteStr(daoAddress.bytes)        -> configAddressReward,
          ByteStr(xtnBuybackAddress.bytes) -> configAddressReward
        ).sortBy(_._1)
      }
    }
  }

  property(
    s"NODE-842, NODE-843. blockInfoByHeight(height) should return correct rewards after ${BlockchainFeatures.CappedReward} activation"
  ) {
    val invoker           = TxHelpers.signer(1)
    val dApp              = TxHelpers.signer(2)
    val daoAddress        = TxHelpers.address(3)
    val xtnBuybackAddress = TxHelpers.address(4)

    val settings = ConsensusImprovements
      .copy(blockchainSettings =
        ConsensusImprovements.blockchainSettings.copy(
          functionalitySettings = ConsensusImprovements.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString)),
          rewardsSettings = ConsensusImprovements.blockchainSettings.rewardsSettings.copy(initial = BlockRewardCalculator.FullRewardInit + 1.waves)
        )
      )
      .setFeaturesHeight(BlockchainFeatures.BlockRewardDistribution -> 3, BlockchainFeatures.CappedReward -> 5)

    val dAppBeforeBlockRewardDistribution = TestCompiler(V7).compileContract(blockInfoScript(V7, "value(blockInfoByHeight(2)).rewards"))
    val dAppAfterBlockRewardDistribution  = TestCompiler(V7).compileContract(blockInfoScript(V7, "value(blockInfoByHeight(3)).rewards"))
    val dAppAfterCappedReward             = TestCompiler(V7).compileContract(blockInfoScript(V7, "value(blockInfoByHeight(5)).rewards"))

    withDomain(settings, balances = AddrWithBalance.enoughBalances(invoker, dApp)) { d =>
      def checkAfterBlockRewardDistrResult(miner: Address, configAddressReward: Long): Assertion = {
        val expectedResAfterBlockRewardDistribution = Seq(
          ByteStr(miner.bytes)             -> (d.blockchain.settings.rewardsSettings.initial - 2 * configAddressReward),
          ByteStr(daoAddress.bytes)        -> configAddressReward,
          ByteStr(xtnBuybackAddress.bytes) -> configAddressReward
        ).sortBy(_._1)
        d.blockchain.accountData(dApp.toAddress, "size") shouldBe Some(IntegerDataEntry("size", 3))
        (1 to 3).map { idx =>
          (
            d.blockchain.accountData(dApp.toAddress, s"addr$idx").get.asInstanceOf[BinaryDataEntry].value,
            d.blockchain.accountData(dApp.toAddress, s"reward$idx").get.asInstanceOf[IntegerDataEntry].value
          )
        } shouldBe expectedResAfterBlockRewardDistribution
      }

      val invoke = () => TxHelpers.invoke(dApp.toAddress, Some("foo"), invoker = invoker)
      val cleanData = () =>
        TxHelpers.data(
          dApp,
          Seq(EmptyDataEntry("size")) ++ (1 to 3).flatMap(idx => Seq(EmptyDataEntry(s"addr$idx"), EmptyDataEntry(s"reward$idx"))),
          version = TxVersion.V2
        )

      val miner = d.appendBlock().sender.toAddress
      d.appendBlock(TxHelpers.setScript(dApp, dAppBeforeBlockRewardDistribution)) // BlockRewardDistribution activation
      d.appendBlockE(invoke()) should beRight
      checkAfterBlockRewardDistrResult(miner, d.blockchain.settings.rewardsSettings.initial / 3)

      d.appendBlockE(cleanData(), invoke()) should beRight // CappedReward activation
      d.blockchain.accountData(dApp.toAddress, "size") shouldBe Some(IntegerDataEntry("size", 1))
      d.blockchain.accountData(dApp.toAddress, "addr1").get.asInstanceOf[BinaryDataEntry].value shouldBe ByteStr(miner.bytes)
      d.blockchain
        .accountData(dApp.toAddress, "reward1")
        .get
        .asInstanceOf[IntegerDataEntry]
        .value shouldBe d.blockchain.settings.rewardsSettings.initial

      (2 to 3).map { idx =>
        d.blockchain.accountData(dApp.toAddress, s"addr$idx") shouldBe None
        d.blockchain.accountData(dApp.toAddress, s"reward$idx") shouldBe None
      }

      d.appendBlockE(TxHelpers.setScript(dApp, dAppAfterBlockRewardDistribution), cleanData()) should beRight
      d.appendBlockE(invoke()) should beRight
      checkAfterBlockRewardDistrResult(miner, d.blockchain.settings.rewardsSettings.initial / 3)

      d.appendBlockE(TxHelpers.setScript(dApp, dAppAfterCappedReward), cleanData()) should beRight
      d.appendBlockE(invoke()) should beRight
      checkAfterBlockRewardDistrResult(miner, BlockRewardCalculator.MaxAddressReward)
    }
  }

  property("transfer transaction by id") {
    val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, transfer2) = preconditionsAndPayments
    setScriptTransactions.foreach { setScriptTransaction =>
      assertDiffAndState(smartEnabledFS) { append =>
        append(genesis).explicitGet()
        append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
        append(Seq(transferTx)).explicitGet()

        val script = ScriptCompiler
          .compile(
            s"""
               | {-# STDLIB_VERSION 3          #-}
               | {-# CONTENT_TYPE   EXPRESSION #-}
               | {-# SCRIPT_TYPE    ACCOUNT    #-}
               |
               | let transfer = extract(
               |   transferTransactionById(base64'${transferTx.id().base64Raw}')
               | )
               |
               | let checkTransferOpt = match transferTransactionById(base64'') {
               |  case _: Unit => true
               |  case _: TransferTransaction => false
               |  case _ => false
               | }
               |
               | let checkAddress = match transfer.recipient {
               |   case addr: Address => addr.bytes == base64'${ByteStr(transferTx.recipient.bytes).base64Raw}'
               |   case _             => false
               | }
               |
               | let checkAmount     = transfer.amount == ${transferTx.amount}
               | let checkAttachment = transfer.attachment == base64'${Base64.encode(transferTx.attachment.arr)}'
               |
               | let checkAssetId = match transfer.assetId {
               |    case _: Unit => true
               |    case _       => false
               | }
               |
               | let checkFeeAssetId = match transfer.feeAssetId {
               |    case _: Unit => true
               |    case _       => false
               | }
               |
               | let checkAnotherTxType = !isDefined(
               |   transferTransactionById(base64'${dataTransaction.id().base64Raw}')
               | )
               |
               | checkTransferOpt    &&
               | checkAmount         &&
               | checkAddress        &&
               | checkAttachment     &&
               | checkAssetId        &&
               | checkFeeAssetId     &&
               | checkAnotherTxType
               |
              """.stripMargin,
            estimator
          )
          .explicitGet()
          ._1

        val setScriptTx = TxHelpers.setScript(masterAcc, script)

        append(Seq(setScriptTx)).explicitGet()
        append(Seq(transfer2)).explicitGet()
      }
    }
  }

  property("account this") {
    val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, transfer2) = preconditionsAndPayments
    setScriptTransactions.foreach { setScriptTransaction =>
      assertDiffAndState(smartEnabledFS) { append =>
        append(genesis).explicitGet()
        append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
        append(Seq(transferTx)).explicitGet()

        val script = ScriptCompiler
          .compile(
            s"""
               | {-# STDLIB_VERSION 3 #-}
               | {-# CONTENT_TYPE EXPRESSION #-}
               | {-# SCRIPT_TYPE ACCOUNT #-}
               |
               | this.bytes == base58'${masterAcc.toAddress}'
               |
              """.stripMargin,
            estimator
          )
          .explicitGet()
          ._1

        val setScriptTx = TxHelpers.setScript(masterAcc, script)

        append(Seq(setScriptTx)).explicitGet()
        append(Seq(transfer2)).explicitGet()
      }
    }
  }

  property("address toString") {
    val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, transfer2) = preconditionsAndPayments
    setScriptTransactions.foreach { setScriptTransaction =>
      assertDiffAndState(smartEnabledFS) { append =>
        append(genesis).explicitGet()
        append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
        append(Seq(transferTx)).explicitGet()

        val script = ScriptCompiler
          .compile(
            s"""
               | {-# STDLIB_VERSION 3 #-}
               | {-# CONTENT_TYPE EXPRESSION #-}
               | {-# SCRIPT_TYPE ACCOUNT #-}
               |
               | let checkAddressToStrRight = this.toString() == "${masterAcc.toAddress}"
               | let checkAddressToStr = this.bytes.toBase58String() == this.toString()
               |
               | checkAddressToStrRight && checkAddressToStr
               |
              """.stripMargin,
            estimator
          )
          .explicitGet()
          ._1

        val setScriptTx = TxHelpers.setScript(masterAcc, script)

        append(Seq(setScriptTx)).explicitGet()
        append(Seq(transfer2)).explicitGet()
      }
    }
  }

  property("transactionFromProtoBytes") {
    val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, transfer2) = preconditionsAndPayments
    setScriptTransactions.foreach { setScriptTransaction =>
      val fs = smartEnabledFS.copy(preActivatedFeatures = smartEnabledFS.preActivatedFeatures + (BlockV5.id -> 0))

      assertDiffAndState(fs) { append =>
        append(genesis).explicitGet()
        append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
        append(Seq(transferTx)).explicitGet()

        val txBytesBase58 = Base58.encode(PBTransactionSerializer.bytes(transferTx))
        val script = ScriptCompiler
          .compile(
            s"""
               |
               | {-# STDLIB_VERSION 4 #-}
               | {-# CONTENT_TYPE EXPRESSION #-}
               | {-# SCRIPT_TYPE ACCOUNT #-}
               |
               | let transferTx  = transferTransactionFromProto(base58'$txBytesBase58').value()
               | let incorrectTx = transferTransactionFromProto(base58'aaaa')
               |
               | incorrectTx          == unit                                                  &&
               | transferTx.id        == base58'${transferTx.id()}'                      &&
               | transferTx.amount    == ${transferTx.amount}                                  &&
               | transferTx.sender    == Address(base58'${transferTx.sender.toAddress}') &&
               | transferTx.recipient == Address(base58'${transferTx.recipient}')
               |
               """.stripMargin,
            estimator
          )
          .explicitGet()
          ._1

        val setScriptTx = SetScriptTransaction
          .selfSigned(1.toByte, masterAcc, Some(script), 1000000L, transferTx.timestamp + 5)
          .explicitGet()

        append(Seq(setScriptTx)).explicitGet()
        append(Seq(transfer2)).explicitGet()
      }
    }
  }

  property("self-state functions") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V5)
      .foreach { version =>
        val (masterAcc, recipient, genesis, _, dataTransaction, transferTx, _) = preconditionsAndPayments
        val fs                                                                 = settingsForRide(version).blockchainSettings.functionalitySettings
        assertDiffAndState(fs) { append =>
          val (intKey, intValue)         = dataTransaction.data.collectFirst { case IntegerDataEntry(key, value) => (key, value) }.get
          val (booleanKey, booleanValue) = dataTransaction.data.collectFirst { case BooleanDataEntry(key, value) => (key, value) }.get
          val (binaryKey, binaryValue)   = dataTransaction.data.collectFirst { case BinaryDataEntry(key, value) => (key, value) }.get
          val (stringKey, stringValue)   = dataTransaction.data.collectFirst { case StringDataEntry(key, value) => (key, value) }.get

          val script =
            s"""
               |  {-# STDLIB_VERSION ${version.id} #-}
               |  {-# CONTENT_TYPE DAPP #-}
               |
               |  @Callable(i)
               |  func default() = {
               |    let result =
               |      getInteger("$intKey")          == $intValue            &&
               |      getIntegerValue("$intKey")     == $intValue            &&
               |      getString("$stringKey")        == "$stringValue"       &&
               |      getStringValue("$stringKey")   == "$stringValue"       &&
               |      getBoolean("$booleanKey")      == $booleanValue        &&
               |      getBooleanValue("$booleanKey") == $booleanValue        &&
               |      getBinary("$binaryKey")        == base58'$binaryValue' &&
               |      getBinaryValue("$binaryKey")   == base58'$binaryValue' &&
               |      getInteger("unexisting")       == unit                 &&
               |      getString("unexisting")        == unit                 &&
               |      getBinary("unexisting")        == unit                 &&
               |      getString("unexisting")        == unit
               |
               |    if (result) then [] else throw("failed")
               |  }
             """.stripMargin
          val expr = Parser.parseContract(script).get.value

          val ctx =
            PureContext.build(version, useNewPowPrecision = true).withEnvironment[Environment] |+|
              CryptoContext.build(Global, version).withEnvironment[Environment] |+|
              WavesContext.build(Global, DirectiveSet(version, Account, DApp).explicitGet(), fixBigScriptField = true)

          val compiledScript = ContractScript(version, ContractCompiler(ctx.compilerContext, expr, version).explicitGet()).explicitGet()
          val setScriptTx    = TxHelpers.setScript(recipient, compiledScript)

          val invoke = TxHelpers.invoke(recipient.toAddress, invoker = masterAcc, version = TxVersion.V1)

          append(genesis).explicitGet()
          append(Seq(dataTransaction)).explicitGet()
          append(Seq(setScriptTx)).explicitGet()
          append(Seq(invoke)).explicitGet()
        }
      }
  }

  property("addressFromPublicKey native") {
    val (masterAcc, _, genesis, setScriptTransactions, dataTransaction, transferTx, transfer2) = preconditionsAndPayments
    val fs = smartEnabledFS.copy(preActivatedFeatures = smartEnabledFS.preActivatedFeatures + (BlockchainFeatures.RideV6.id -> 0))

    setScriptTransactions.foreach { setScriptTransaction =>
      assertDiffAndState(fs) { append =>
        append(genesis).explicitGet()
        append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
        append(Seq(transferTx)).explicitGet()

        val script = TestCompiler(V6)
          .compileExpression(
            s"""
               | addressFromPublicKey(base58'${transferTx.sender}') == Address(base58'${transferTx.sender.toAddress}') &&
               | addressFromPublicKey(base58'$TestEthOrdersPublicKey') == Address(base58'${TestEthOrdersPublicKey.toAddress}')
           """.stripMargin
          )

        val setScriptTx = SetScriptTransaction
          .selfSigned(1.toByte, masterAcc, Some(script), 1000000L, transferTx.timestamp + 5)
          .explicitGet()

        append(Seq(setScriptTx)).explicitGet()
        append(Seq(transfer2)).explicitGet()
      }
    }
  }

  property("transactionHeightById and transactionById returns only succeed transactions") {
    withDomain(RideV6, AddrWithBalance.enoughBalances(secondSigner, signer(2), signer(3))) { d =>
      val failingDApp = TestCompiler(V6).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict c = ${(1 to 6).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
           |   if (true) then throw() else []
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, failingDApp))
      val failedInvoke = invoke()

      val checkTransactionHeightById = TestCompiler(V6).compileExpression(s"transactionHeightById(base58'${failedInvoke.id().toString}').isDefined()")
      val checkTransactionById       = TestCompiler(V2).compileExpression(s"transactionById(base58'${failedInvoke.id().toString}').isDefined()")

      d.appendBlock(setScript(signer(2), checkTransactionHeightById))
      d.appendBlock(setScript(signer(3), checkTransactionById))

      d.appendMicroBlock(failedInvoke)
      d.appendMicroBlockE(transfer(signer(2))) should produce("TransactionNotAllowedByScript")
      d.appendMicroBlockE(transfer(signer(3))) should produce("TransactionNotAllowedByScript")
      d.appendKeyBlock()
      d.appendBlockE(transfer(signer(2))) should produce("TransactionNotAllowedByScript")
      d.appendBlockE(transfer(signer(3))) should produce("TransactionNotAllowedByScript")
    }
  }

  private def blockInfoScript(version: StdLibVersion, getRewardsCode: String): String =
    s"""
       |{-# STDLIB_VERSION ${version.id} #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |
       | @Callable(i)
       | func foo() = {
       |   let r = $getRewardsCode
       |   let s = r.size()
       |   let first = if (s >= 1) then [BinaryEntry("addr1", r[0]._1.bytes), IntegerEntry("reward1", r[0]._2)] else []
       |   let second = if (s >= 2) then [BinaryEntry("addr2", r[1]._1.bytes), IntegerEntry("reward2", r[1]._2)] else []
       |   let third = if (s >= 3) then [BinaryEntry("addr3", r[2]._1.bytes), IntegerEntry("reward3", r[2]._2)] else []
       |   
       |   [IntegerEntry("size", s)] ++ first ++ second ++ third
       | }
       |""".stripMargin
}
