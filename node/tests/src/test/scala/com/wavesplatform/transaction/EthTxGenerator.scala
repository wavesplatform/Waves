package com.wavesplatform.transaction

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.transaction.TransactionType.Transfer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.utils.EthEncoding
import org.web3j.abi.FunctionEncoder.{buildMethodId, buildMethodSignature}
import org.web3j.abi.datatypes.{AbiTypes, StructType}
import org.web3j.abi.{DefaultFunctionEncoder, FunctionEncoder, TypeEncoder}
import org.web3j.crypto.*

import java.math.BigInteger
import java.util

object EthTxGenerator {
  sealed trait Arg
  object Arg {
    case class Integer(v: Long, typeStr: String = "int64")        extends Arg
    case class Bytes(v: ByteStr, typeStr: String = "bytes")       extends Arg
    case class Str(v: String)                                     extends Arg
    case class BigInteger(bi: BigInt, typeStr: String = "int256") extends Arg
    case class Bool(b: Boolean)                                   extends Arg
    case class List(listType: Arg, elements: Seq[Arg])            extends Arg
    case class Struct(values: Arg*)                               extends Arg
  }

  import org.web3j.abi.datatypes as ethTypes

  def toEthType(value: Arg): ethTypes.Type[?] = value match {
    case Arg.Integer(v, typeStr) =>
      val typeClass = ethTypes.AbiTypes.getType(typeStr)
      typeClass.getConstructor(classOf[Long]).newInstance(v)
    case Arg.BigInteger(bi, typeStr) =>
      val typeClass = ethTypes.AbiTypes.getType(typeStr)
      typeClass.getConstructor(classOf[java.math.BigInteger]).newInstance(bi.bigInteger)
    case Arg.Str(v) =>
      new ethTypes.Utf8String(v)
    case Arg.Bytes(v, typeStr) =>
      val typeClass = ethTypes.AbiTypes.getType(typeStr)
      typeClass.getConstructor(classOf[Array[Byte]]).newInstance(v.arr)
    case Arg.Bool(b) =>
      new ethTypes.Bool(b)
    case Arg.List(listType, elements) =>
      val ethTypedXs = elements.map(toEthType)
      val arrayClass = toEthType(listType)
      new ethTypes.DynamicArray(arrayClass.getClass.asInstanceOf[Class[ethTypes.Type[?]]], ethTypedXs*) {
        override def getTypeAsString: String =
          (if (classOf[StructType].isAssignableFrom(arrayClass.getClass)) arrayClass.getTypeAsString
           else AbiTypes.getTypeAString(getComponentType)) + "[]"
      }
    case Arg.Struct(values*) => new ethTypes.StaticStruct(values.map(toEthType)*)
  }

  def signRawTransaction(keyPair: ECKeyPair, chainId: Byte)(raw: RawTransaction): EthereumTransaction = {
    val signedTx =
      new SignedRawTransaction(
        raw.getTransaction,
        TransactionEncoder.createEip155SignatureData(Sign.signMessage(TransactionEncoder.encode(raw, chainId.toLong), keyPair, true), chainId.toLong)
      )
    EthereumTransaction(signedTx).explicitGet()
  }

  def generateEthTransfer(
      keyPair: ECKeyPair,
      recipient: Address,
      amount: Long,
      asset: Asset,
      fee: Long = FeeConstants(Transfer) * FeeUnit,
      withRedundantBytes: Boolean = false
  ): EthereumTransaction = asset match {
    case Asset.Waves =>
      signRawTransaction(keyPair, recipient.chainId)(
        RawTransaction.createTransaction(
          BigInt(System.currentTimeMillis()).bigInteger,
          EthereumTransaction.GasPrice,
          BigInt(fee).bigInteger, // fee
          EthEncoding.toHexString(recipient.publicKeyHash),
          (BigInt(amount) * EthereumTransaction.AmountMultiplier).bigInteger,
          ""
        )
      )

    case Asset.IssuedAsset(assetId) =>
      import scala.jdk.CollectionConverters.*
      val function = new org.web3j.abi.datatypes.Function(
        "transfer",
        Seq[ethTypes.Type[?]](
          new ethTypes.Address(EthEncoding.toHexString(recipient.publicKeyHash)),
          new ethTypes.generated.Uint256(amount)
        ).asJava,
        Nil.asJava
      )

      val additionalData = if (withRedundantBytes) "aa" else ""

      signRawTransaction(keyPair, recipient.chainId)(
        RawTransaction.createTransaction(
          BigInt(System.currentTimeMillis()).bigInteger, // nonce
          EthereumTransaction.GasPrice,
          BigInt(fee).bigInteger,                           // fee
          EthEncoding.toHexString(assetId.arr.take(20)),    // to (asset erc20 "contract" address)
          FunctionEncoder.encode(function) + additionalData // data
        )
      )
  }

  def generateEthInvoke(
      keyPair: ECKeyPair,
      address: Address,
      funcName: String,
      args: Seq[Arg],
      payments: Seq[Payment],
      fee: Long = 500000,
      withRedundantBytes: Boolean = false
  ): EthereumTransaction = {
    import scala.jdk.CollectionConverters.*
    val paymentsArg = {
      val tuples = payments.toVector.map { p =>
        val assetId = p.assetId match {
          case Asset.IssuedAsset(id) => id
          case Asset.Waves           => EthABIConverter.WavesByteRepr
        }
        Arg.Struct(Arg.Bytes(assetId, "bytes32"), Arg.Integer(p.amount))
      }
      Arg.List(Arg.Struct(Arg.Bytes(EthABIConverter.WavesByteRepr, "bytes32"), Arg.Integer(0)), tuples)
    }

    val fullArgs = args :+ paymentsArg

    val argsAsEth = fullArgs.map(toEthType)
    val function = new org.web3j.abi.datatypes.Function(
      funcName,
      argsAsEth.asJava,
      Nil.asJava
    )

    val encodedFunction = if (withRedundantBytes) {
      redundantBytesFunctionEncoder.encodeFunction(function)
    } else {
      FunctionEncoder.encode(function)
    }

    signRawTransaction(keyPair, address.chainId)(
      RawTransaction.createTransaction(
        BigInt(System.currentTimeMillis()).bigInteger,
        EthereumTransaction.GasPrice,
        BigInt(fee).bigInteger,
        EthEncoding.toHexString(address.publicKeyHash),
        encodedFunction
      )
    )
  }

  private def redundantBytesFunctionEncoder: DefaultFunctionEncoder = new DefaultFunctionEncoder {
    import org.web3j.abi.datatypes as ethTypes

    override def encodeFunction(function: ethTypes.Function): String = {
      val parameters = function.getInputParameters

      val methodSignature = buildMethodSignature(function.getName, parameters)
      val methodId        = buildMethodId(methodSignature)

      val result = new StringBuilder(methodId)

      encodeParameters(parameters, result)
    }

    private def isDynamic(parameter: ethTypes.Type[?]) =
      parameter.isInstanceOf[ethTypes.DynamicBytes] || parameter.isInstanceOf[ethTypes.Utf8String] || parameter
        .isInstanceOf[ethTypes.DynamicArray[?]] || (parameter
        .isInstanceOf[ethTypes.StaticArray[?]] && classOf[ethTypes.DynamicStruct].isAssignableFrom(
        parameter.asInstanceOf[ethTypes.StaticArray[?]].getComponentType
      ))

    private def encodeParameters(parameters: util.List[ethTypes.Type[?]], result: StringBuilder): String = {
      var dynamicDataOffset = getLength(parameters) * ethTypes.Type.MAX_BYTE_LENGTH + 1
      val dynamicData       = new StringBuilder
      import scala.jdk.CollectionConverters.*
      for (parameter <- parameters.asScala) {
        val encodedValue = TypeEncoder.encode(parameter)
        if (isDynamic(parameter)) {
          val encodedDataOffset = TypeEncoder.encode(new ethTypes.Uint(BigInteger.valueOf(dynamicDataOffset)))
          result.append(encodedDataOffset)
          dynamicData.append(encodedValue)
          dynamicDataOffset += encodedValue.length >> 1
        } else result.append(encodedValue)
      }
      result.append("aa")
      result.append(dynamicData)
      result.toString
    }

    private def getLength(parameters: util.List[ethTypes.Type[?]]) = {
      val cls    = Class.forName("org.web3j.abi.DefaultFunctionEncoder")
      val method = cls.getDeclaredMethod("getLength", classOf[util.List[ethTypes.Type[?]]])
      method.setAccessible(true)
      method.invoke(this, parameters).asInstanceOf[Int]
    }
  }
}
