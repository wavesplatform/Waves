package com.wavesplatform.matchers.functions

import com.wavesplatform.matchers._
import com.wavesplatform.matchers.ExprMatcher.If
import com.wavesplatform.matchers.PositionMatcher.StartEndMatch

trait IfFunctions { self: PosMatcherFunctions with PartFunctions =>
  val anyIf = If(anyPos, anyExpr, anyExpr, anyExpr)

  def ifExpr(cond: ExprMatcher): If                                            = If(anyPos, cond, anyExpr, anyExpr)
  def ifExpr(cond: ExprMatcher, ifTrue: ExprMatcher): If                       = If(anyPos, cond, ifTrue, anyExpr)
  def ifExpr(cond: ExprMatcher, ifTrue: ExprMatcher, ifFalse: ExprMatcher): If = If(anyPos, cond, ifTrue, ifFalse)

  def ifExpr(start: Int, end: Int, cond: ExprMatcher): If                      = If(StartEndMatch(start, end), cond, anyExpr, anyExpr)
  def ifExpr(start: Int, end: Int, cond: ExprMatcher, ifTrue: ExprMatcher): If = If(StartEndMatch(start, end), cond, ifTrue, anyExpr)
  def ifExpr(start: Int, end: Int, cond: ExprMatcher, ifTrue: ExprMatcher, ifFalse: ExprMatcher): If =
    If(StartEndMatch(start, end), cond, ifTrue, ifFalse)

  def ifExpr(positionMatcher: PositionMatcher, cond: ExprMatcher): If                      = If(positionMatcher, cond, anyExpr, anyExpr)
  def ifExpr(positionMatcher: PositionMatcher, cond: ExprMatcher, ifTrue: ExprMatcher): If = If(positionMatcher, cond, ifTrue, anyExpr)
  def ifExpr(positionMatcher: PositionMatcher, cond: ExprMatcher, ifTrue: ExprMatcher, ifFalse: ExprMatcher): If =
    If(positionMatcher, cond, ifTrue, ifFalse)
}
