package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.{Global, StdLibVersion, ContentType}
import com.wavesplatform.lang.StdLibVersion.V1
import com.wavesplatform.lang.Testing._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.utils.compilerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import shapeless.Coproduct

class ContextFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  def compactDataTransactionGen(sender: PrivateKeyAccount) =
    for {
      long <- longEntryGen(dataAsciiKeyGen)
      bool <- booleanEntryGen(dataAsciiKeyGen).filter(_.key != long.key)
      bin  <- binaryEntryGen(40, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key)
      str  <- stringEntryGen(40, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key && e.key != bin.key)
      tx   <- dataTransactionGenP(sender, List(long, bool, bin, str))
    } yield tx

  val preconditionsAndPayments = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis1 = GenesisTransaction.create(master, ENOUGH_AMT * 3, ts).explicitGet()
    genesis2 = GenesisTransaction.create(recipient, ENOUGH_AMT * 3, ts).explicitGet()
    dataTransaction <- compactDataTransactionGen(recipient)
    transfer        <- transferGeneratorP(ts, master, recipient.toAddress, 100000000L)

    untypedScript <- Gen
      .choose(1, 3)
      .map {
        case 1 => scriptWithPureFunctions(dataTransaction, transfer)
        case 2 => scriptWithWavesFunctions(dataTransaction, transfer)
        case 3 => scriptWithCryptoFunctions
      }
      .map(x => Parser.parseExpr(x).get.value)

    typedScript = {
      val compilerScript = ExpressionCompiler(compilerContext(V1, ContentType.Expression, isAssetScript = false), untypedScript).explicitGet()._1
      ExprScript(compilerScript).explicitGet()
    }
    setScriptTransaction: SetScriptTransaction = SetScriptTransaction.selfSigned(recipient, Some(typedScript), 100000000L, ts).explicitGet()

  } yield (Seq(genesis1, genesis2), setScriptTransaction, dataTransaction, transfer)

  property("validation of all functions from contexts") {
    forAll(preconditionsAndPayments) {
      case (genesis, setScriptTransaction, dataTransaction, transfer) =>
        assertDiffAndState(smartEnabledFS) { append =>
          append(genesis).explicitGet()
          append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
          append(Seq(transfer)).explicitGet()
        }
    }
  }

  property("reading from data transaction array by key") {
    forAll(preconditionsAndPayments) {
      case (_, _, tx, _) =>
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
               |  let int  = extract(getInteger(d, "${int.key}"))
               |  let bool = extract(getBoolean(d, "${bool.key}"))
               |  let bin  = extract(getBinary(d, "${bin.key}"))
               |  let str  = extract(getString(d, "${str.key}"))
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
               |  let okIntV  = int + 1  == ${int.value} + 1
               |  let okBoolV = bool || true == ${bool.value} || true
               |  let okBinV  = bin  == base58'${Base58.encode(bin.asInstanceOf[BinaryDataEntry].value.arr)}'
               |  let okStrV  = str + ""  == "${str.value}"
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
          StdLibVersion.V3
        )
        result shouldBe evaluated(true)
    }
  }

  property("reading from data transaction array by index") {
    forAll(preconditionsAndPayments, Gen.choose(4, 40)) {
      case ((_, _, tx, _), badIndex) =>
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
        outOfBounds shouldBe Left(s"java.lang.IndexOutOfBoundsException: $badIndex")
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
    runScript(script) shouldBe Left(s"base64Encode input exceeds ${Global.MaxBase64Bytes}")
  }
}
