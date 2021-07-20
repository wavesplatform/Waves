package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.{Terms, Types}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import org.web3j.abi.datatypes.Type
import org.web3j.abi.{FunctionReturnDecoder, TypeReference}
import play.api.libs.json.{JsArray, JsObject, Json}

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
    import org.web3j.abi.{datatypes => dt}

    import scala.jdk.CollectionConverters._

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

                case _ => ???
              }
          }

        case _ => Nil
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
    JsArray(functionsWithArgs.map {
      case (funcName, args) =>
        val paymentsArg = Json.obj("name" -> "payments", "type" -> ABIConverter.ethTypeObj(ABIConverter.PaymentListType))

        val inputs = args.map {
          case (argName, argType) =>
            Json.obj("name" -> argName, "type" -> ABIConverter.ethTypeObj(argType))
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
    })

  def decodeFunctionCall(data: String): (FUNCTION_CALL, Seq[InvokeScriptTransaction.Payment]) = {
    val (methodId, argsData) = data.splitAt(8)
    val function             = funcByMethodId.getOrElse(methodId, throw new NoSuchElementException(s"Function not defined: $methodId"))
    val (args, payment)      = function.decodeArgs(argsData)
    (FUNCTION_CALL(FunctionHeader.User(function.name), args), payment)
  }
}