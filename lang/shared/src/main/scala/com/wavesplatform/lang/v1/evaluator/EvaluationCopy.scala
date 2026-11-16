package com.wavesplatform.lang.v1.evaluator

import com.wavesplatform.lang.v1.compiler.Terms.*

/** The copy of a script tree the evaluator runs on, since it writes results into the tree. */
object EvaluationCopy {
  // Marks the node below it on the work stack as one whose children are all copied.
  private object Rebuild

  /** Copies only what the evaluator writes into before reaching it, in a loop over explicit stacks. Leaves and function declarations are
    * shared; a LET is a new node whose value is shared and marked `valueShared`. See docs/notes/ride-evaluator.md.
    */
  def apply(root: EXPR): EXPR = {
    val work   = new java.util.ArrayDeque[AnyRef]()
    val copies = new java.util.ArrayDeque[AnyRef]()

    def copy[A]: A = copies.pop().asInstanceOf[A]

    def isShared(e: AnyRef): Boolean = e match {
      case _: REF | _: FAILED_EXPR | _: EVALUATED | _: FAILED_DEC | _: FUNC => true
      case _                                                                => false
    }

    def copyLet(l: LET): LET = {
      val copied = LET(l.name, l.value)
      copied.valueShared = !isShared(l.value)
      copied
    }

    work.push(root)
    while (!work.isEmpty) work.pop() match {
      case Rebuild =>
        val copied = work.pop() match {
          case g: GETTER     => GETTER(copy, g.field)
          case lb: LET_BLOCK => LET_BLOCK(copyLet(lb.let), copy)
          case _: BLOCK =>
            val body = copy[EXPR]
            BLOCK(copy, body)
          case _: IF =>
            val ifFalse = copy[EXPR]
            val ifTrue  = copy[EXPR]
            IF(copy, ifTrue, ifFalse)
          case fc: FUNCTION_CALL =>
            var args = List.empty[EXPR]
            fc.args.foreach(_ => args = copy[EXPR] :: args)
            FUNCTION_CALL(fc.function, args)
          case other => throw new IllegalStateException(s"Unexpected node $other")
        }
        copies.push(copied)
      case shared if isShared(shared) =>
        copies.push(shared)
      case l: LET =>
        copies.push(copyLet(l))
      case node =>
        work.push(node)
        work.push(Rebuild)
        // Children go on in reverse, so they are copied first to last and come off the copies stack last to first.
        node match {
          case g: GETTER         => work.push(g.expr)
          case lb: LET_BLOCK     => work.push(lb.body)
          case b: BLOCK          => work.push(b.body); work.push(b.dec)
          case i: IF             => work.push(i.ifFalse); work.push(i.ifTrue); work.push(i.cond)
          case fc: FUNCTION_CALL => fc.args.reverseIterator.foreach(work.push)
          case other             => throw new IllegalStateException(s"Unexpected node $other")
        }
    }
    copies.pop().asInstanceOf[EXPR]
  }
}
