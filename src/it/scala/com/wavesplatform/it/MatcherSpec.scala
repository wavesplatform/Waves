package com.wavesplatform.it

import org.scalatest.{FreeSpec, Matchers}

class MatcherSpec(nodes: Seq[Node]) extends FreeSpec with Matchers {
  private val matcherNode = nodes.head
  private val aliceNode = nodes.tail
}
