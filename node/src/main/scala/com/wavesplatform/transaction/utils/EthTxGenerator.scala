package com.wavesplatform.transaction.utils

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.transaction.{Asset, EthereumTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.utils.EthEncoding
import org.web3j.abi.FunctionEncoder
import org.web3j.abi.datatypes.{AbiTypes, StructType}
import org.web3j.crypto._

// TODO: Move to separate package available to node-generator and node:test
object EthTxGenerator {
  sealed trait Arg
  object Arg {
    case class Integer(v: Long, typeStr: String = "int64")        extends Arg
    case class Bytes(v: ByteStr)                                  extends Arg
    case class Str(v: String)                                     extends Arg
    case class BigInteger(bi: BigInt, typeStr: String = "int256") extends Arg
    case class Bool(b: Boolean)                                   extends Arg
    case class List(listType: Arg, elements: Seq[Arg])            extends Arg
    case class Union(index: Int, fields: Seq[Arg])                extends Arg
    case class Struct(values: Arg*)                               extends Arg
  }

  import org.web3j.abi.{datatypes => ethTypes}
  // import com.esaulpaugh.headlong.{abi => hl}
  // import scala.jdk.CollectionConverters._

  //noinspection ScalaDeprecation
  /* def toEthType(value: Terms.EVALUATED): ethTypes.Type[_] = value match { // TODO remove
    case Terms.CONST_LONG(t)     => new ethTypes.generated.Int64(t)
    case Terms.CONST_BIGINT(t)   => new ethTypes.generated.Int256(t.bigInteger)
    case bs: Terms.CONST_BYTESTR => new ethTypes.DynamicBytes(bs.bs.arr)
    case s: Terms.CONST_STRING   => new ethTypes.Utf8String(s.s)
    case Terms.CONST_BOOLEAN(b)  => new ethTypes.Bool(b)
    case ARR(xs) =>
      val ethTypedXs = xs.map(toEthType)
      val arrayClass = (ethTypedXs.headOption match {
        case Some(value) =>
          if (classOf[StructType].isAssignableFrom(value.getClass)) value.getClass
          else ethTypes.AbiTypes.getType(value.getTypeAsString)

        case None => classOf[ethTypes.BytesType]
      }).asInstanceOf[Class[ethTypes.Type[_]]]

      new ethTypes.DynamicArray(arrayClass, ethTypedXs: _*) {
        override def getTypeAsString: String = {
          val head = ethTypedXs.headOption.getOrElse(ethTypes.generated.Bytes32.DEFAULT)
          (if (classOf[StructType].isAssignableFrom(head.getClass)) head.getTypeAsString else AbiTypes.getTypeAString(getComponentType)) + "[]"
        }
      }

    case CaseObj(_, fields) => new ethTypes.DynamicStruct(fields.values.toSeq.map(toEthType): _*)
    case _                  => ???
  } */

  def toEthType(value: Arg): ethTypes.Type[_] = value match {
    case Arg.Integer(v, typeStr)     => ethTypes.AbiTypes.getType(typeStr).getConstructor(classOf[Long]).newInstance(v)
    case Arg.BigInteger(bi, typeStr) => ethTypes.AbiTypes.getType(typeStr).getConstructor(classOf[java.math.BigInteger]).newInstance(bi.bigInteger)
    case Arg.Str(v)                  => new ethTypes.Utf8String(v)
    case Arg.Bytes(v)                => new ethTypes.DynamicBytes(v.arr)
    case Arg.Bool(b)                 => new ethTypes.Bool(b)
    case Arg.List(listType, elements) =>
      val ethTypedXs = elements.map(toEthType)
      val arrayClass = toEthType(listType)
      new ethTypes.DynamicArray(arrayClass.getClass.asInstanceOf[Class[ethTypes.Type[_]]], ethTypedXs: _*) {
        override def getTypeAsString: String =
          (if (classOf[StructType].isAssignableFrom(arrayClass.getClass)) arrayClass.getTypeAsString else AbiTypes.getTypeAString(getComponentType)) + "[]"
      }
    case Arg.Union(index, fields) =>
      new ethTypes.DynamicStruct(toEthType(Arg.Integer(index, "uint8")) +: fields.map(toEthType): _*)

    case Arg.Struct(values @ _*) => new ethTypes.DynamicStruct(values.map(toEthType): _*)
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

  def signRawTransaction(keyPair: ECKeyPair, chainId: Byte)(raw: RawTransaction): EthereumTransaction = {
    val signedTx =
      new SignedRawTransaction(raw.getTransaction, Sign.signMessage(TransactionEncoder.encode(raw, chainId.toLong), keyPair, true))
    EthereumTransaction(signedTx).explicitGet()
  }

  def generateEthTransfer(keyPair: ECKeyPair, recipient: Address, amount: Long, asset: Asset): EthereumTransaction = asset match {
    case Asset.Waves =>
      signRawTransaction(keyPair, recipient.chainId)(RawTransaction.createTransaction(
        BigInt(System.currentTimeMillis()).bigInteger,
        EthereumTransaction.GasPrice,
        BigInt(100000).bigInteger, // fee
        EthEncoding.toHexString(recipient.publicKeyHash),
        (BigInt(amount) * EthereumTransaction.AmountMultiplier).bigInteger,
        ""
      ))

    case Asset.IssuedAsset(assetId) =>
      import scala.jdk.CollectionConverters._
      val function = new org.web3j.abi.datatypes.Function(
        "transfer",
        Seq[ethTypes.Type[_]](
          new ethTypes.Address(EthEncoding.toHexString(recipient.publicKeyHash)),
          new ethTypes.generated.Uint256(amount)
        ).asJava,
        Nil.asJava
      )

      signRawTransaction(keyPair, recipient.chainId)(RawTransaction.createTransaction(
        BigInt(System.currentTimeMillis()).bigInteger,
        EthereumTransaction.GasPrice,
        BigInt(100000).bigInteger, // fee
        EthEncoding.toHexString(assetId.arr.take(20)), // asset erc20 "contract" address
        FunctionEncoder.encode(function)
      ))
  }

  def generateEthInvoke(keyPair: ECKeyPair, address: Address, funcName: String, args: Seq[Arg], payments: Seq[Payment]): EthereumTransaction = {
    import scala.jdk.CollectionConverters._
    val paymentsArg = {
      val tuples = payments.toVector.map { p =>
        val assetId = p.assetId match {
          case Asset.IssuedAsset(id) => id
          case Asset.Waves           => ByteStr.empty // ByteStr(new Array[Byte](32))
        }
        Arg.Struct(Arg.Bytes(assetId), Arg.Integer(p.amount))
      }
      Arg.List(Arg.Struct(Arg.Bytes(ByteStr.empty), Arg.Integer(0)), tuples)
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

    signRawTransaction(keyPair, address.chainId)(RawTransaction.createTransaction(
      BigInt(System.currentTimeMillis()).bigInteger,
      EthereumTransaction.GasPrice,
      BigInt(500000).bigInteger, // fee
      EthEncoding.toHexString(address.publicKeyHash),
      FunctionEncoder.encode(function)
    ))
  }
}
