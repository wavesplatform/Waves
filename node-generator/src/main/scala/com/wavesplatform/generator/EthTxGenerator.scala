package com.wavesplatform.generator

import scala.collection.immutable.TreeMap

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CaseObj}
import com.wavesplatform.transaction.{Asset, EthereumTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.utils.EthEncoding
import org.web3j.abi.FunctionEncoder
import org.web3j.crypto._

object EthTxGenerator {
  import org.web3j.abi.{datatypes => ethTypes}
  // import com.esaulpaugh.headlong.{abi => hl}
  // import scala.jdk.CollectionConverters._

  //noinspection ScalaDeprecation
  def toEthType(value: Terms.EVALUATED): ethTypes.Type[_] = value match {
    case Terms.CONST_LONG(t)     => new ethTypes.generated.Int64(t)
    case Terms.CONST_BIGINT(t)   => new ethTypes.generated.Int256(t.bigInteger)
    case bs: Terms.CONST_BYTESTR => new ethTypes.DynamicBytes(bs.bs.arr)
    case s: Terms.CONST_STRING   => new ethTypes.Utf8String(s.s)
    case Terms.CONST_BOOLEAN(b)  => new ethTypes.Bool(b)
    case ARR(xs)                 => new ethTypes.DynamicArray(xs.map(toEthType): _*)
    /* case CaseObj(CASETYPEREF("Address", _, _), fields) =>
      val wavesAddress = Address.fromBytes(fields.head._2.asInstanceOf[Terms.CONST_BYTESTR].bs.arr).explicitGet()
      new ethTypes.Address(wavesAddress.toEthAddress) */
    case CaseObj(_, fields) => new ethTypes.DynamicStruct(fields.values.toSeq.map(toEthType): _*)
    case _                  => ???
  }

  /* def toHLEthType(value: Terms.EVALUATED): hl.ABIType[_] = value match {
    case Terms.CONST_LONG(_)    => hl.TypeFactory.create[hl.LongType]("int64")
    case Terms.CONST_BIGINT(_)  => hl.TypeFactory.create[hl.BigIntegerType]("int256")
    case Terms.CONST_BOOLEAN(_) => hl.TypeFactory.create[hl.BooleanType]("bool")
    case _: Terms.CONST_BYTESTR => hl.TypeFactory.create[hl.ArrayType[hl.ByteType, Array[Byte]]]("bytes")
    case _: Terms.CONST_STRING  => hl.TypeFactory.create[hl.ArrayType[hl.ByteType, String]]("string")
    case ARR(xs)                => hl.TypeFactory.create[hl.ArrayType[_, _]](s"${toHLEthType(xs.head)}[]")
    case CaseObj(_, fields)     => hl.TypeFactory.create[hl.TupleType](fields.values.map(toHLEthType).mkString("(", ",", ")"))
  }

  def toHLEthValue(value: Terms.EVALUATED): Any = value match {
    case Terms.CONST_LONG(t)     => java.lang.Long.valueOf(t)
    case Terms.CONST_BIGINT(t)   => t.bigInteger
    case bs: Terms.CONST_BYTESTR => bs.bs.arr
    case s: Terms.CONST_STRING   => s.s
    case Terms.CONST_BOOLEAN(b)  => java.lang.Boolean.valueOf(b)
    case ARR(xs)                 =>
      val values = xs.map(toHLEthValue)
      values.toArray(ClassTag(values.head.getClass))
    case CaseObj(_, fields)      => hl.Tuple.of(fields.values.toSeq.map(toHLEthValue): _*)
    case _                       => ???
  } */

  def generateEthInvoke(
      keyPair: ECKeyPair,
      address: Address,
      chainId: Byte,
      funcName: String,
      args: Seq[Terms.EVALUATED],
      payments: Seq[Payment]
  ): EthereumTransaction = {
    import scala.jdk.CollectionConverters._
    val paymentsArg = {
      val tuples = payments.toVector.map(
        p =>
          CaseObj(
            Terms.runtimeTupleType,
            TreeMap(
              "_1" -> Terms
                .CONST_BYTESTR(p.assetId match {
                  case Asset.IssuedAsset(id) => id
                  case Asset.Waves           => ByteStr.empty // ByteStr(new Array[Byte](32))
                })
                .explicitGet(),
              "_2" -> Terms.CONST_LONG(p.amount)
            )
          )
      )
      Terms.ARR(tuples, limited = false).explicitGet()
    }

    val fullArgs = (args :+ paymentsArg)

    /* val ethFunc = new hl.Function(
      hl.TypeEnum.FUNCTION,
      funcName,
      hl.TupleType.of(fullArgs.map(a => toHLEthType(a).getCanonicalType): _*),
      hl.TupleType.EMPTY,
      null,
      hl.Function.newDefaultDigest()
    )
    val values      = fullArgs.map(toHLEthValue)
    val ethCallData = EthEncoding.toHexString(ethFunc.encodeCallWithArgs(values:_*).array()) */

    val function = new org.web3j.abi.datatypes.Function(
      funcName,
      fullArgs.map(toEthType).asJava,
      Nil.asJava
    )

    val raw = RawTransaction.createTransaction(
      BigInt(System.currentTimeMillis()).bigInteger,
      BigInt(1).bigInteger,
      BigInt(500000).bigInteger, // fee
      EthEncoding.toHexString(address.publicKeyHash),
      FunctionEncoder.encode(function)
    )

    val signedTx = new SignedRawTransaction(raw.getTransaction, Sign.signMessage(TransactionEncoder.encode(raw, chainId.toLong), keyPair, true))
    EthereumTransaction(signedTx)
  }
}
