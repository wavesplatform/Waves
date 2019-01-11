import cats.kernel.Monoid
import shapeless.{:+:, CNil}
import com.wavesplatform.lang.{Global, ScriptVersion}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.traits.domain.{Ord, Recipient, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.evaluator.ctx.impl._

import scala.collection.JavaConverters._
import com.github.mustachejava._

object DocExport {
  def main(args: Array[String]) {
    if (args.size != 4 || args(0) != "--gen-doc") {
      System.err.println("Expected args: --gen-doc <version> <template> <output>")
    } else {
      val version = ScriptVersion.fromInt(args(1).toByte).get
      val wavesContext = WavesContext.build(
        version,
        new Environment {
          override def height: Long                                                                                    = ???
          override def networkByte: Byte                                                                               = 66
          override def inputEntity: Tx :+: Ord :+: CNil                                                                = ???
          override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
          override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
          override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]                   = ???
          override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
          override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
        },
        isTokenContext = true
      )

      val cryptoContext = CryptoContext.build(Global)

      abstract class TypeDoc {
        val name: String
        val isUnion: Boolean   = false
        val isObj: Boolean     = false
        val isNative: Boolean  = false
        val haveParam: Boolean = false
        val isComplex: Boolean = false
        val needLink: Boolean  = false
        def noLink: Boolean    = !needLink
      }
      case class Field(name: String, `type`: TypeDoc)
      case class UnionDoc(override val name: String, union: java.util.List[TypeDoc]) extends TypeDoc { override val isUnion: Boolean = true }
      case class objDoc(override val name: String, fields: java.util.List[Field]) extends TypeDoc {
        override val isObj: Boolean = true; override val needLink: Boolean = true
      }
      case class ListOf(param: TypeDoc)   extends TypeDoc { override val name: String = "LIST"; val hasParam: Boolean   = true }
      case class OptionOf(param: TypeDoc) extends TypeDoc { override val name: String = "OPTION"; val hasParam: Boolean = true }
      case class nativeTypeDoc(override val name: String) extends TypeDoc {
        override val isNative: Boolean = true; override val needLink: Boolean = true
      }

      def typeRepr(t: FINAL)(name: String = t.name): TypeDoc = t match {
        case UNION(Seq(UNIT, l)) => OptionOf(typeRepr(l)())
        case UNION(Seq(l, UNIT)) => OptionOf(typeRepr(l)())
        case UNION(l)            => UnionDoc(name, l.map(t => typeRepr(t)()).asJava)
        case CASETYPEREF(_, fields) =>
          objDoc(name, fields.map(f => Field(f._1, typeRepr(f._2)())).asJava)
        case LIST(t) => ListOf(typeRepr(t)())
        case t       => nativeTypeDoc(t.toString)
      }

      val fullContext: CTX = Monoid.combineAll(Seq(PureContext.build(version), cryptoContext, wavesContext))

      def getTypes() = fullContext.types.map(v => typeRepr(v.typeRef)(v.name))

      case class VarDoc(name: String, `type`: TypeDoc, doc: String)
      def getVarsDoc() = fullContext.vars.map(v => VarDoc(v._1, typeRepr(v._2._1._1)(), v._2._1._2))

      case class FuncDoc(name: String, `type`: TypeDoc, doc: String, params: java.util.List[VarDoc], cost: String)

      def extType(t: TYPE): TypeDoc = t match {
        case t: FINAL                         => typeRepr(t)()
        case TYPEPARAM(char: Byte)            => new TypeDoc { val name: String = char.toChar.toString; override val isComplex: Boolean = true }
        case PARAMETERIZEDLIST(l)             => ListOf(extType(l))
        case PARAMETERIZEDUNION(Seq(UNIT, l)) => OptionOf(extType(l))
        case PARAMETERIZEDUNION(Seq(l, UNIT)) => OptionOf(extType(l))
        case PARAMETERIZEDUNION(l)            => UnionDoc("", l.map(t => extType(t)).asJava)
        case t                                => new TypeDoc { val name: String = t.toString; override val isComplex: Boolean = true }
      }

      def getFunctionnsDoc() =
        fullContext.functions
          .map(
            f =>
              FuncDoc(
                f.name,
                extType(f.signature.result),
                f.docString,
                ((f.argsDoc zip f.signature.args) map { arg =>
                  VarDoc(arg._1._1, extType(arg._2._2), arg._1._2)
                }).toList.asJava,
                f match {
                  case NativeFunction(_, cost, _, _, _, _) => cost.toString
                  case _                                   => ""
                }
            ))

      case class TransactionDoc(name: String, fields: java.util.List[TransactionField])
      case class TransactionField(absend: Boolean, `type`: java.util.List[TypeDoc])
      case class FieldTypes(name: String, types: java.util.List[TransactionField])
      val transactionsType       = fullContext.types.filter(v => v.name == "Transaction")
      val transactionsTypesNames = transactionsType.flatMap({ case UnionType(_, union) => union.map(_.name) }).toSet
      def transactionDocs(types: Seq[DefinedType], fieldsFlt: String => Boolean = (_ => true)) = {
        val transactionsTypes =
          types.flatMap({
            case UnionType(_, union) => union
            case t: CaseType         => Seq(t.typeRef)
            case t                   => println(t.toString); Seq()
          })
        val transactionsFields =
          transactionsTypes
            .flatMap(_.fields.map(_._1))
            .filter(fieldsFlt)
            .distinct
            .map(name =>
              FieldTypes(
                name,
                transactionsTypes
                  .map(t =>
                    t.fields
                      .find(_._1 == name)
                      .fold(TransactionField(true, List[TypeDoc]().asJava))(ft => TransactionField(false, List(typeRepr(ft._2)()).asJava)))
                  .asJava
            ))
        val transactionDocs = transactionsTypes.map { t =>
          val fields = t.fields.toMap
          TransactionDoc(
            t.name,
            transactionsFields
              .map(
                field =>
                  fields
                    .get(field.name)
                    .fold(TransactionField(true, List[TypeDoc]().asJava))(ft => TransactionField(false, List(typeRepr(ft)()).asJava)))
              .asJava
          )
        }
        (transactionDocs, transactionsFields)
      }

      val commonFields              = Set("id", "fee", "version", "timestamp", "sender", "senderPublicKey", "bodyBytes", "proofs")
      def otherFields(name: String) = !commonFields(name)

      case class CaseDoc(types: java.util.List[TransactionDoc], fields: java.util.List[FieldTypes])
      def caseDoc(d: (Seq[TransactionDoc], Seq[FieldTypes])) = CaseDoc(d._1.asJava, d._2.asJava)
      case class Special(`class`: String, descr: CaseDoc)
      case class Doc(types: java.util.List[TypeDoc],
                     vars: java.util.List[VarDoc],
                     funcs: java.util.List[FuncDoc],
                     transactionDoc: java.util.List[TransactionDoc],
                     transactionFields: java.util.List[FieldTypes],
                     commonFields: CaseDoc,
                     specials: java.util.List[Special])

      val mf      = new DefaultMustacheFactory()
      val doc     = mf.compile(args(2))
      val output  = new java.io.FileWriter(args(3)) //new java.io.StringWriter
      val (t, f)  = transactionDocs(transactionsType)
      val commons = transactionDocs(transactionsType, commonFields)
      val transactionClasses = Seq(
        "Transfers"      -> Set("TransferTransaction", "MassTransferTransaction", "PaymentTransaction"),
        "Issuing assets" -> Set("IssueTransaction", "ReissueTransaction", "BurnTransaction", "SponsorFeeTransaction", "SetAssetScriptTransaction"),
        "Leasing"        -> Set("LeaseTransaction", "LeaseCancelTransaction")
      )
      def otherTransactions(name: String) = transactionsTypesNames(name) && !transactionClasses.map(_._2).exists(_(name))
      val out = doc.execute(
        output,
        Doc(
          getTypes().asJava,
          getVarsDoc().toList.asJava,
          getFunctionnsDoc().toList.asJava,
          t.asJava,
          f.asJava,
          CaseDoc(commons._1.asJava, commons._2.asJava),
          (transactionClasses.map(c => Special(c._1, caseDoc(transactionDocs(fullContext.types.filter(v => c._2(v.name)), otherFields))))
            :+ Special("Other", caseDoc(transactionDocs(fullContext.types.filter(v => otherTransactions(v.name)), otherFields)))).asJava
        )
      )
      out.flush()
      out.close()
    }
  }
}
