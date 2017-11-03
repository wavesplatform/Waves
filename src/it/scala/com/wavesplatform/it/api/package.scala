package com.wavesplatform.it

import java.time.Instant

import com.wavesplatform.it.api.NodeApi.Block
import org.asynchttpclient.Response
import play.api.libs.json.Format
import play.api.libs.json.Json.parse

import scala.concurrent.{ExecutionContext, Future}

package object api {

  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext): Future[A] = f.map(r => parse(r.getResponseBody).as[A])(ec)
  }

  def allBlocks(nodes: Seq[Node])(implicit ec: ExecutionContext): Future[Map[Node, Seq[Block]]] = {
    def aux(node: Node): Future[(Node, Seq[Block])] = {
      for {
        h <- node.height
        blocks <- node.blockSeq(1, h)
      } yield node -> blocks
    }

    Future.sequence(nodes.map(aux)).map(_.toMap)
  }

  def dumpBlocks(nodes: Seq[Node])(implicit ec: ExecutionContext): Future[String] = {
    allBlocks(nodes).map {
      _.map {
        case ((node, blocks)) =>
          val formattedBlocks = blocks
            .map { b =>
              s"${b.height}: t=${Instant.ofEpochMilli(System.currentTimeMillis())} s=${b.signature.trim}, g=${b.generator.trim}"
            }
            .mkString("\n")

          s"""Node: ${node.nodeName}
             |Blocks:
             |$formattedBlocks""".stripMargin
      }.mkString("\n")
    }
  }
}
