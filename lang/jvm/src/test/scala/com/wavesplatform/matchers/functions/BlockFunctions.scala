package com.wavesplatform.matchers.functions

import com.wavesplatform.lang.v1.parser.Expressions.LET
import com.wavesplatform.matchers.ExprMatcher.{Block, Let}
import com.wavesplatform.matchers.PositionMatcher.StartEndMatch
import com.wavesplatform.matchers._

trait BlockFunctions { self: PosMatcherFunctions =>
  val anyBlock = Block(anyPos, any[LET], anyExpr)

  def block(body: ExprMatcher): Block                                   = Block(anyPos, any[LET], body)
  def block(start: Int, end: Int, body: ExprMatcher): Block             = Block(StartEndMatch(start, end), any[LET], body)
  def block(positionMatcher: PositionMatcher, body: ExprMatcher): Block = Block(positionMatcher, any[LET], body)

  def block(let: Let): Block                                   = Block(anyPos, let, anyExpr)
  def block(start: Int, end: Int, let: Let): Block             = Block(StartEndMatch(start, end), let, anyExpr)
  def block(positionMatcher: PositionMatcher, let: Let): Block = Block(positionMatcher, let, anyExpr)

  def block(let: Let, body: ExprMatcher): Block                                   = Block(anyPos, let, body)
  def block(start: Int, end: Int, let: Let, body: ExprMatcher): Block             = Block(StartEndMatch(start, end), let, body)
  def block(positionMatcher: PositionMatcher, let: Let, body: ExprMatcher): Block = Block(positionMatcher, let, body)

}
