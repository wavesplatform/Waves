package com.wavesplatform.matchers.functions

import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.matchers.ExprMatcher.BinaryOp
import com.wavesplatform.matchers.PositionMatcher.StartEndMatch
import com.wavesplatform.matchers.{ExprMatcher, PositionMatcher, anyExpr}

trait BinaryOpFunctions { self: PosMatcherFunctions =>
  def binaryOp(kind: BinaryOperation): BinaryOp                                   = BinaryOp(anyPos, anyExpr, kind, anyExpr)
  def binaryOp(start: Int, end: Int, kind: BinaryOperation): BinaryOp             = BinaryOp(StartEndMatch(start, end), anyExpr, kind, anyExpr)
  def binaryOp(positionMatcher: PositionMatcher, kind: BinaryOperation): BinaryOp = BinaryOp(positionMatcher, anyExpr, kind, anyExpr)

  def binaryOp(kind: BinaryOperation, left: ExprMatcher, right: ExprMatcher): BinaryOp = BinaryOp(anyPos, left, kind, right)
  def binaryOp(start: Int, end: Int, kind: BinaryOperation, left: ExprMatcher, right: ExprMatcher): BinaryOp =
    BinaryOp(StartEndMatch(start, end), left, kind, right)
  def binaryOp(positionMatcher: PositionMatcher, kind: BinaryOperation, left: ExprMatcher, right: ExprMatcher): BinaryOp =
    BinaryOp(positionMatcher, left, kind, right)
}
