package com.wavesplatform.utils.doc

import com.wavesplatform.common.utils.EitherExt2
import com.github.mustachejava.DefaultMustacheFactory
import com.wavesplatform.DocSource
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.directives.values.{Account, Expression, StdLibVersion}
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL, UNION}

import scala.collection.JavaConverters._

object DocExport {
  def main(args: Array[String]) {
    if (args.length != 4 || args(0) != "--gen-doc") {
      System.err.println("Expected args: --gen-doc <version> <template> <output>")
    } else {
      val versionStr  = args(1)
      val docTemplate = args(2)
      val outputFile  = args(3)

      val version = DirectiveDictionary[StdLibVersion].idMap(versionStr.toInt)
      val fullContext = RideFullContext.build(DirectiveSet(version, Account, Expression).explicitGet())

      def getTypes() = fullContext.types.map(v => TypeDoc.typeRepr(v)(v.name))

      def getVarsDoc() = fullContext.vars.map(v => VarDoc(
        v._1,
        TypeDoc.typeRepr(v._2._1)(),
        DocSource.varData((v._1, version.value.asInstanceOf[Int]))
      ))

      def getFunctionsDoc() =
        fullContext.functions
          .map(
            f => {
              val (funcDoc, paramsDoc) = DocSource.funcData((
                f.name,
                f.signature.args.map(_._2.toString).toList,
                version.value.asInstanceOf[Int]
              ))
              FuncDoc(
                f.name,
                TypeDoc(f.signature.result),
                funcDoc,
                ((f.args, f.signature.args, paramsDoc).zipped.toList
                  map { arg => VarDoc(arg._1, TypeDoc(arg._2._2), arg._3)}
                ).asJava,
                f.costByLibVersion(version).toString
              )
            })

      val transactionsType       = fullContext.types.filter(v => v.name == "Transaction")
      val transactionsTypesNames = transactionsType.collect({ case UNION(union, _) => union.map(_.name) }).flatten.toSet
      def transactionDocs(types: Seq[FINAL], fieldsFlt: String => Boolean = (_ => true)) = {
        val transactionsTypes =
          types.flatMap({
            case UNION(union, _) => union
            case t: CASETYPEREF  => Seq(t)
            case t               => println(t.toString); Seq()
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
                      .fold(TransactionField(true, List[TypeDoc]().asJava))(ft => TransactionField(false, List(TypeDoc.typeRepr(ft._2)()).asJava)))
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
                    .fold(TransactionField(true, List[TypeDoc]().asJava))(ft => TransactionField(false, List(TypeDoc.typeRepr(ft)()).asJava)))
              .asJava
          )
        }
        (transactionDocs, transactionsFields)
      }

      val commonFields              = Set("id", "fee", "version", "timestamp", "sender", "senderPublicKey", "bodyBytes", "proofs")
      def otherFields(name: String) = !commonFields(name)

      val mf      = new DefaultMustacheFactory()
      val doc     = mf.compile(docTemplate)
      val output  = new java.io.FileWriter(outputFile) //new java.io.StringWriter
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
          getFunctionsDoc().toList.asJava,
          t.asJava,
          f.asJava,
          CaseDoc(commons._1.asJava, commons._2.asJava),
          (transactionClasses.map(c => Special(c._1, Doc(transactionDocs(fullContext.types.filter(v => c._2(v.name)), otherFields))))
            :+ Special("Other", Doc(transactionDocs(fullContext.types.filter(v => otherTransactions(v.name)), otherFields)))).asJava
        )
      )

      out.flush()
      out.close()
    }
  }
}
