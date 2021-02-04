package com.wavesplatform.state.diffs.smart.predef

import cats.kernel.Monoid
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, FeeSponsorship}
import com.wavesplatform.lagonaki.mocks.TestBlock._
import com.wavesplatform.lang.Testing._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{FunctionHeader, compiler}
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxVersion}
import com.wavesplatform.utils._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import shapeless.Coproduct

class ContextFunctionsTest extends PropSpec with PropertyChecks with WithState with TransactionGen with NoShrink {

  def compactDataTransactionGen(sender: KeyPair): Gen[DataTransaction] =
    for {
      long <- longEntryGen(dataAsciiKeyGen)
      bool <- booleanEntryGen(dataAsciiKeyGen).filter(_.key != long.key)
      bin  <- binaryEntryGen(40, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key)
      str  <- stringEntryGen(40, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key && e.key != bin.key)
      tx   <- dataTransactionGenP(sender, List(long, bool, bin, str))
    } yield tx

  private val preconditionsAndPayments = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis1 = GenesisTransaction.create(master.toAddress, ENOUGH_AMT * 3, ts).explicitGet()
    genesis2 = GenesisTransaction.create(recipient.toAddress, ENOUGH_AMT * 3, ts).explicitGet()
    dataTransaction <- compactDataTransactionGen(recipient)
    transfer        <- transferGeneratorP(ts, master, recipient.toAddress, 100000000L)
    transfer2       <- transferGeneratorPV2(ts + 15, master, recipient.toAddress, 100000L)

    untypedScript <- Gen
      .choose(1, 3)
      .map {
        case 1 => scriptWithV1PureFunctions(dataTransaction, transfer)
        case 2 => scriptWithV1WavesFunctions(dataTransaction, transfer)
        case 3 => scriptWithCryptoFunctions
      }
      .map(x => Parser.parseExpr(x).get.value)

    typedScript = {
      val compilerScript = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untypedScript).explicitGet()._1
      ExprScript(compilerScript).explicitGet()
    }
    setScriptTransaction: SetScriptTransaction = SetScriptTransaction.selfSigned(1.toByte, recipient, Some(typedScript), 100000000L, ts).explicitGet()

  } yield (master, Seq(genesis1, genesis2), setScriptTransaction, dataTransaction, transfer, transfer2)

  private val estimator = ScriptEstimatorV2

  property("validation of all functions from contexts") {
    forAll(preconditionsAndPayments) {
      case (_, genesis, setScriptTransaction, dataTransaction, transfer, _) =>
        assertDiffAndState(smartEnabledFS) { append =>
          append(genesis).explicitGet()
          append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
          append(Seq(transfer)).explicitGet()
        }
    }
  }

  property("reading from data transaction array by key") {
    forAll(for {
      version       <- Gen.oneOf(DirectiveDictionary[StdLibVersion].all.filter(_ >= V3))
      preconditions <- preconditionsAndPayments
    } yield (version, preconditions)) {
      case (version, (_, _, _, tx, _, _)) =>
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
    forAll(preconditionsAndPayments, Gen.choose(4, 40)) {
      case ((_, _, _, tx, _, _), badIndex) =>
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
  }

  property("base64 amplification") {
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
    forAll(for {
      (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2) <- preconditionsAndPayments
      version                                                                            <- Gen.oneOf(DirectiveDictionary[StdLibVersion].all.filter(_ >= V3).toSeq)
      v4Activation                                                                       <- if (version >= V4) Gen.const(true) else Gen.oneOf(false, true)
    } yield (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2, version, v4Activation)) {
      case (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2, version, v4Activation) =>
        val fs = {
          val features = smartEnabledFS.copy(preActivatedFeatures = smartEnabledFS.preActivatedFeatures + (FeeSponsorship.id -> 0))
          if (v4Activation) features.copy(preActivatedFeatures = features.preActivatedFeatures + (BlockV5.id -> 0))
          else features
        }

        assertDiffAndState(fs) { append =>
          append(genesis).explicitGet()
          append(Seq(setScriptTransaction, dataTransaction)).explicitGet()

          val quantity    = 100000000L
          val decimals    = 6.toByte
          val reissuable  = true
          val assetScript = None

          val issueTx = IssueTransaction(
            TxVersion.V2,
            masterAcc.publicKey,
            "testAsset".utf8Bytes,
            "Test asset".utf8Bytes,
            quantity,
            decimals,
            reissuable,
            assetScript,
            MinIssueFee * 2,
            dataTransaction.timestamp + 5
          ).signWith(masterAcc.privateKey)

          val sponsoredFee = 100
          val sponsorTx =
            SponsorFeeTransaction
              .signed(
                TxVersion.V1,
                masterAcc.publicKey,
                issueTx.asset,
                Some(sponsoredFee),
                MinIssueFee,
                dataTransaction.timestamp + 6,
                masterAcc.privateKey
              )
              .explicitGet()

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

          val setScriptTx = SetScriptTransaction.selfSigned(1.toByte, masterAcc, Some(script), 1000000L, issueTx.timestamp + 5).explicitGet()

          append(Seq(sponsorTx)).explicitGet()
          append(Seq(setScriptTx)).explicitGet()
          append(Seq(transfer2)).explicitGet()
        }
    }
  }

  property("last block info check") {
    forAll(preconditionsAndPayments) {
      case (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2) =>
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
                 | let lastBlockGenerator = lastBlock.generator.bytes == base58'${defaultSigner.publicKey.toAddress}'
                 | let lastBlockGeneratorPublicKey = lastBlock.generatorPublicKey == base58'${defaultSigner.publicKey}'
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
    forAll(for {
      (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2) <- preconditionsAndPayments
      version                                                                            <- Gen.oneOf(DirectiveDictionary[StdLibVersion].all.filter(_ >= V3).toSeq)
      withVrf                                                                            <- Gen.oneOf(version >= V4, true)
    } yield (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2, version, withVrf)) {
      case (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2, version, withVrf) =>
        val generationSignature =
          if (withVrf) ByteStr(new Array[Byte](Block.GenerationVRFSignatureLength)) else ByteStr(new Array[Byte](Block.GenerationSignatureLength))

        val fs =
          if (version >= V4) smartEnabledFS.copy(preActivatedFeatures = smartEnabledFS.preActivatedFeatures + (BlockV5.id -> 0))
          else smartEnabledFS

        val fsWithVrf =
          if (withVrf) fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockV5.id -> 0))
          else fs

        val (v4DeclOpt, v4CheckOpt) =
          if (version >= V4)
            if (withVrf)
              (s"let checkVrf = block.vrf != unit", "&& checkVrf")
            else
              (s"let checkVrf = block.vrf == unit", "&& checkVrf")
          else ("", "")

        assertDiffAndState(fsWithVrf) { append =>
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
                 | let checkGenerator = block.generator.bytes == base58'${defaultSigner.publicKey.toAddress}'
                 | let checkGeneratorPublicKey = block.generatorPublicKey == base58'${defaultSigner.publicKey}'
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

          val setScriptTx = SetScriptTransaction.selfSigned(1.toByte, masterAcc, Some(script), 1000000L, transferTx.timestamp + 5).explicitGet()

          append(Seq(setScriptTx)).explicitGet()
          append(Seq(transfer2)).explicitGet()
        }

    }
  }

  property("blockInfoByHeight(height) is the same as lastBlock") {
    forAll(preconditionsAndPayments) {
      case (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, _) =>
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
          val expr = Parser.parseContract(script).get.value

          val ctx = {
            utils.functionCosts(V3)
            Monoid
              .combineAll(
                Seq(
                  PureContext.build(V3).withEnvironment[Environment],
                  CryptoContext.build(Global, V3).withEnvironment[Environment],
                  WavesContext.build(
                    DirectiveSet(V3, Account, Expression).explicitGet()
                  )
                )
              )
          }

          val compiledScript = ContractScript(V3, compiler.ContractCompiler(ctx.compilerContext, expr, V3).explicitGet()).explicitGet()
          val setScriptTx =
            SetScriptTransaction.selfSigned(1.toByte, masterAcc, Some(compiledScript), 1000000L, transferTx.timestamp + 5).explicitGet()
          val fc = Terms.FUNCTION_CALL(FunctionHeader.User("compareBlocks"), List.empty)

          val ci = InvokeScriptTransaction
            .selfSigned(
              1.toByte,
              masterAcc,
              masterAcc.toAddress,
              Some(fc),
              Seq.empty,
              FeeValidation.FeeUnit * (FeeValidation.FeeConstants(InvokeScriptTransaction.typeId) + FeeValidation.ScriptExtraFee),
              Waves,
              System.currentTimeMillis()
            )
            .explicitGet()

          append(Seq(setScriptTx)).explicitGet()
          append(Seq(ci)).explicitGet()
        }
    }
  }

  property("transfer transaction by id") {
    forAll(preconditionsAndPayments) {
      case (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2) =>
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

          val setScriptTx = SetScriptTransaction
            .selfSigned(1.toByte, masterAcc, Some(script), 1000000L, transferTx.timestamp + 5)
            .explicitGet()

          append(Seq(setScriptTx)).explicitGet()
          append(Seq(transfer2)).explicitGet()
        }
    }
  }

  property("account this") {
    forAll(preconditionsAndPayments) {
      case (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2) =>
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

          val setScriptTx = SetScriptTransaction.selfSigned(1.toByte, masterAcc, Some(script), 1000000L, transferTx.timestamp + 5).explicitGet()

          append(Seq(setScriptTx)).explicitGet()
          append(Seq(transfer2)).explicitGet()
        }
    }
  }

  property("address toString") {
    forAll(preconditionsAndPayments) {
      case (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2) =>
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

          val setScriptTx = SetScriptTransaction
            .selfSigned(1.toByte, masterAcc, Some(script), 1000000L, transferTx.timestamp + 5)
            .explicitGet()

          append(Seq(setScriptTx)).explicitGet()
          append(Seq(transfer2)).explicitGet()
        }
    }
  }

  property("transactionFromProtoBytes") {
    forAll(preconditionsAndPayments) {
      case (masterAcc, genesis, setScriptTransaction, dataTransaction, transferTx, transfer2) =>
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
}
