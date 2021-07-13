package com.wavesplatform.transaction

import java.math.BigInteger

import scala.reflect.ClassTag

import com.wavesplatform.account.{Address, AddressScheme, EthereumAddress, PublicKey, Recipient}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.{Terms, Types}
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FUNCTION_CALL}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.{Height, TxNum}
import com.wavesplatform.state.diffs.invoke.{InvokeScriptLike, InvokeScriptTransactionLike}
import com.wavesplatform.transaction.EthereumTransaction.ABIConverter.ethTypeObj
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import monix.eval.Coeval
import org.bouncycastle.util.encoders.Hex
import org.web3j.abi.{FunctionReturnDecoder, TypeDecoder, TypeReference}
import org.web3j.abi.datatypes.{Type, Address => EthAddress}
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.crypto._
import org.web3j.rlp.{RlpEncoder, RlpList}
import org.web3j.utils.Numeric._
import play.api.libs.json._

sealed abstract class EthereumTransaction(final val underlying: SignedRawTransaction) extends Transaction(TransactionType.Ethereum) {
  private final val signatureData: Sign.SignatureData = underlying.getSignatureData
  override val bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(RlpEncoder.encode(new RlpList(TransactionEncoder.asRlpValues(underlying, signatureData))))

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    underlying.getEncodedTransaction(AddressScheme.current.chainId.toLong)
  }

  override val id: Coeval[ByteStr] = bytes.map(bs => ByteStr(Hash.sha3(bs)))

  override def assetFee: (Asset, Long) = Asset.Waves -> underlying.getGasLimit.longValueExact()

  override val timestamp: TxTimestamp = underlying.getNonce.longValueExact()

  override val protoSize: Coeval[Int] = bytes.map(_.length)

  override val chainId: Byte = underlying.getChainId.byteValue()

  val signerPublicKey: Coeval[Array[Byte]] = bodyBytes.map { bs =>
    Sign
      .recoverFromSignature(
        1,
        new ECDSASignature(new BigInteger(1, signatureData.getR), new BigInteger(1, signatureData.getS)),
        Hash.sha3(bs)
      )
      .toByteArray
  }

  val signatureValid: Coeval[Boolean] = signerPublicKey.map(_ => true)

  val baseJson: Coeval[JsObject] = for {
    idValue <- id
  } yield Json.obj(
    "id"                  -> idValue.toString,
    "type"                -> tpe.id,
    "ethereumTransaction" -> ethereumJson(None, None, None)
  )

  def ethereumJson(blockId: Option[BlockId], height: Option[Height], num: Option[TxNum]): JsObject = Json.obj(
    "blockHash"        -> blockId.map(id => toHexString(id.arr)),
    "blockNumber"      -> height.map(h => toHexStringWithPrefix(BigInteger.valueOf(h))),
    "from"             -> new EthereumAddress(Keys.getAddress(signerPublicKey())).toString,
    "gas"              -> toHexStringWithPrefix(underlying.getGasLimit),
    "gasPrice"         -> toHexStringWithPrefix(underlying.getGasPrice),
    "hash"             -> toHexString(id().arr),
    "input"            -> underlying.getData,
    "nonce"            -> toHexStringWithPrefix(underlying.getNonce),
    "to"               -> underlying.getTo,
    "transactionIndex" -> num.map(n => toHexStringWithPrefix(BigInteger.valueOf(n))),
    "value"            -> toHexStringWithPrefix(underlying.getValue),
    "v"                -> toHexString(underlying.getSignatureData.getV),
    "r"                -> toHexString(underlying.getSignatureData.getR),
    "s"                -> toHexString(underlying.getSignatureData.getS)
  )
}

object EthereumTransaction {
  val AmountMultiplier = 10000000000L

  private val decodeMethod = {
    val m = classOf[TypeDecoder].getDeclaredMethod("decode", classOf[String], classOf[Int], classOf[Class[_]])
    m.setAccessible(true)
    m
  }

  private def decode[A](source: String, offset: Int)(implicit ct: ClassTag[A]): A =
    decodeMethod.invoke(null, source, offset, ct.runtimeClass.asInstanceOf[Class[A]]).asInstanceOf[A]

  object ABIConverter {
    val PaymentListType: Types.LIST = Types.LIST(Types.TUPLE(List(Types.BYTESTR, Types.LONG)))

    private def buildMethodId(str: String): String = {
      val cls    = Class.forName("org.web3j.abi.FunctionEncoder")
      val method = cls.getDeclaredMethod("buildMethodId", classOf[String])
      method.setAccessible(true)
      method.invoke(null, str).asInstanceOf[String]
    }

    def ethType(argType: Types.FINAL): String =
      (ethTypeObj(argType) \ "type").as[String]

    def ethTypeObj(argType: Types.FINAL): JsObject = {
      def t(s: String) = Json.obj("type" -> s)

      argType match {
        case Types.LONG    => t("int64")
        case Types.BIGINT  => t("int256")
        case Types.BYTESTR => t("bytes")
        case Types.BOOLEAN => t("bool")
        case Types.STRING  => t("string")
        case Types.LIST(innerType) =>
          val base = ethTypeObj(innerType)
          base ++ Json.obj("type" -> (base.value("type").as[String] + "[]"))
        case Types.TUPLE(types) =>
          t("tuple") ++ Json.obj("components" -> types.zipWithIndex.map { case (t, i) => ethTypeObj(t) ++ Json.obj("name" -> s"_$i") })
        case _ => ???
      }
    }

    def toRideValue(ethValue: Type[_]): EVALUATED = {
      import scala.jdk.CollectionConverters._

      import org.web3j.abi.{datatypes => dt}

      ethValue match {
        case bool: dt.Bool                   => Terms.CONST_BOOLEAN(bool.getValue)
        case bytes: dt.Bytes                 => Terms.CONST_BYTESTR(ByteStr(bytes.getValue)).explicitGet()
        case str: dt.Utf8String              => Terms.CONST_STRING(str.getValue).explicitGet()
        case i: dt.Int if i.getBitSize <= 64 => Terms.CONST_LONG(i.getValue.longValueExact())
        case i: dt.NumericType               => Terms.CONST_BIGINT(i.getValue)
        case t: dt.Array[_]                  => Terms.ARR(t.getValue.asScala.map(toRideValue).toVector, limited = true).explicitGet()
        case _                               => throw new UnsupportedOperationException(s"Type not supported: $ethValue")
      }
    }
  }

  final case class ABIConverter(script: Script) {
    case class FunctionArg(name: String, rideType: Types.FINAL) {
      lazy val ethType: String               = ABIConverter.ethType(rideType)
      def ethTypeRef: TypeReference[Type[_]] = TypeReference.makeTypeReference(ethType).asInstanceOf[TypeReference[Type[_]]]
    }

    case class FunctionRef(name: String, args: Seq[FunctionArg]) {
      def decodeArgs(data: String): (List[EVALUATED], Seq[InvokeScriptTransaction.Payment]) = {
        import scala.jdk.CollectionConverters._
        val argsWithPayment = this.args :+ FunctionArg("payments", ABIConverter.PaymentListType)
        val result          = FunctionReturnDecoder.decode(data, argsWithPayment.map(_.ethTypeRef).asJava).asScala.map(ABIConverter.toRideValue).toList
        val payment = result.last match {
          case Terms.ARR(xs) =>
            xs.map {
              case Terms.CaseObj(_, fields) =>
                fields.values.toSeq match {
                  case Seq(Terms.CONST_BYTESTR(assetId), Terms.CONST_LONG(amount)) =>
                    InvokeScriptTransaction.Payment(amount, assetId match {
                      case ByteStr.empty => Asset.Waves
                      case assetId       => Asset.IssuedAsset(assetId)
                    })
                }
            }
        }
        (result.init, payment)
      }

      lazy val ethSignature: String = {
        val argTypes = args.map { case FunctionArg(_, argType) => ABIConverter.ethType(argType) } :+ ABIConverter.ethType(
          ABIConverter.PaymentListType
        )
        s"$name(${argTypes.mkString(",")})"
      }

      lazy val ethMethodId: String = ABIConverter.buildMethodId(ethSignature)
    }

    private[this] lazy val funcsWithTypes = Global.dAppFuncTypes(script)

    private[this] def functionsWithArgs: Seq[(String, List[(String, Types.FINAL)])] = funcsWithTypes match {
      case Right(signatures) => signatures.argsWithFuncName
      case Left(_)           => Nil
    }

    lazy val funcByMethodId: Map[String, FunctionRef] = functionsWithArgs
      .map {
        case (funcName, args) =>
          FunctionRef(funcName, args.map { case (name, argType) => FunctionArg(name, argType) })
      }
      .map(func => func.ethMethodId -> func)
      .toMap

    def jsonABI: JsArray =
      Json.arr(functionsWithArgs.map {
        case (funcName, args) =>
          val paymentsArg = Json.obj("name" -> "payments", "type" -> ethTypeObj(ABIConverter.PaymentListType))

          val inputs = args.map {
            case (argName, argType) =>
              Json.obj("name" -> argName, "type" -> ethTypeObj(argType))
          } :+ paymentsArg

          Json.obj(
            "name"            -> funcName,
            "type"            -> "function",
            "constant"        -> false,
            "payable"         -> true,
            "stateMutability" -> "payable",
            "inputs"          -> inputs,
            "outputs"         -> Json.arr(Json.obj("name" -> "", "type" -> "bool"))
          )
      }: _*)

    def decodeFunctionCall(data: String): (FUNCTION_CALL, Seq[InvokeScriptTransaction.Payment]) = {
      val (methodId, argsData) = data.splitAt(8)
      val function             = funcByMethodId.getOrElse(methodId, throw new NoSuchElementException(s"Function not defined: $methodId"))
      val (args, payment)      = function.decodeArgs(argsData)
      (FUNCTION_CALL(FunctionHeader.User(function.name), args), payment)
    }
  }

  class Transfer(
      val sender: Address,
      val asset: Either[Asset.Waves.type, ERC20Address],
      val amount: TxAmount,
      val recipient: EthereumAddress,
      underlying: SignedRawTransaction
  ) extends EthereumTransaction(underlying) {
    override val json: Coeval[JsObject] = baseJson.map(
      _ ++ Json.obj(
        "transfer" -> Json.obj(
          "sender"    -> sender.asWaves.toString,
          "recipient" -> recipient.toString,
          "amount"    -> amount,
          "asset" -> (asset match {
            case Left(_)      => JsNull
            case Right(erc20) => toHexString(erc20.arr)
          })
        )
      )
    )
  }

  class InvokeScript(
      val senderAddress: Address,
      val dApp: Recipient,
      val callData: ByteStr,
      underlying: SignedRawTransaction
  ) extends EthereumTransaction(underlying) {
    private[this] def hexCallData: String = Hex.toHexString(callData.arr)

    final class Invokable(script: Script) extends InvokeScriptTransactionLike {
      lazy val (funcCall, payments)             = ABIConverter(script).decodeFunctionCall(hexCallData)
      def dApp: Recipient                       = InvokeScript.this.dApp
      def root: Option[InvokeScriptTransaction] = None
      def senderAddress: Address                = InvokeScript.this.senderAddress
      def sender: PublicKey                     = PublicKey(signerPublicKey())
      def id: Coeval[BlockId]                   = InvokeScript.this.id
      val (feeAssetId, fee)                     = InvokeScript.this.assetFee
      def transaction: InvokeScript             = InvokeScript.this
    }

    def toInvokable(script: Script): Invokable = new Invokable(script)

    override val json: Coeval[JsObject] = baseJson.map(
      _ ++ Json.obj("invokeScript" -> Json.obj("sender" -> senderAddress.asWaves.toString, "dApp" -> dApp.toString, "callData" -> hexCallData))
    )
  }

  def apply(bytes: Array[Byte]): EthereumTransaction =
    apply(TransactionDecoder.decode(toHexString(bytes)).asInstanceOf[SignedRawTransaction])

  val ERC20TransferPrefix: String = "a9059cbb"

  def apply(underlying: SignedRawTransaction): EthereumTransaction = {
    val hexData          = cleanHexPrefix(underlying.getData)
    val senderAddress    = PBRecipients.toAddress(hexStringToByteArray(underlying.getFrom), underlying.getChainId.toByte).explicitGet()
    val recipientAddress = ByteStr(hexStringToByteArray(underlying.getTo))
    if (hexData.isEmpty) {
      new Transfer(
        senderAddress,
        Left(Asset.Waves),
        underlying.getValue.divide(BigInt(AmountMultiplier).bigInteger).longValueExact(),
        new EthereumAddress(recipientAddress.arr),
        underlying
      )
    } else if (hexData.startsWith(ERC20TransferPrefix)) {
      val recipient = decode[EthAddress](hexData, 8)
      val amount    = decode[Uint256](hexData, 72)
      new Transfer(
        senderAddress,
        Right(ERC20Address(recipientAddress)),
        amount.getValue.longValueExact(),
        new EthereumAddress(hexStringToByteArray(recipient.toString)),
        underlying
      )
    } else new InvokeScript(senderAddress, new EthereumAddress(recipientAddress.arr), ByteStr(Hex.decode(hexData)), underlying)
  }
}
