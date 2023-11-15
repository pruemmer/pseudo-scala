/**
 * This file is part of Pseudo-Scala.
 * <https://github.com/pruemmer/pseudo-scala>
 *
 * Copyright (C) 2023 Philipp Ruemmer <ph_r@gmx.net>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * 
 * * Neither the name of the authors nor the names of their
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package pseudoscala

import scala.util.DynamicVariable

/**
 * Functions controlling non-deterministic execution.
 */
object nondet {

  private val ndSearch = new DynamicVariable[NDSearch](null)

  trait ValueEnum[T] {
    def iterator : Iterator[T]
  }

  implicit object BooleanEnum extends ValueEnum[Boolean] {
    def iterator = Iterator(false, true)
  }

  /**
   * Start a non-deterministic computation, implemented using
   * backtracking.
   */
  def btSearch[A, Result](comp : => A) : Option[Result] = {
    val s = new BacktrackingSearch
    ndSearch.withValue(s) {
      s.btSearch(comp)
    }
  }

  /**
   * Start a non-deterministic computation, implemented using
   * repeated execution.
   */
  def search[Result](comp : => Result) : Option[Result] = {
    val s = new TracingSearch
    ndSearch.withValue(s) {
      s.search(comp)
    }
  }

  private def withNDSearch[A](comp : NDSearch => A) : A = {
    val s = ndSearch.value
    assert(s != null, "No non-deterministic execution has been started")
    comp(s)
  }

  /**
   * Non-deterministically choose a value of type <code>T</code> and continue
   * program execution.
   */
  def choose[T,R](comp : T => R)(implicit venum : ValueEnum[T]) : R =
    withNDSearch(s => s.choose(comp))

  class RichComp[A](comp : => A) {
    def □(comp2 : => A) : A =
      choose { (v : Boolean) => if (v) comp2 else comp }
  }

  implicit def toRichComp[A](comp : => A) : RichComp[A] = new RichComp(comp)

  /**
   * Non-deterministically choose a value in the given integer range and
   * continue program execution.
   */
  def chooseInt[R](r : Range)(comp : Int => R) : R =
    withNDSearch(s => s.chooseInt(r)(comp))

  /**
   * Mark that computation at this point can take <code>n</code> alternative
   * paths. <code>comp</comp> will usually be written as a <code>match</code>
   * block.
   */
  def alternatives[R](n : Int)(comp : Int => R) : R =
    withNDSearch(s => s.alternatives(n)(comp))

  /**
   * Assume that the given condition is true, block program execution
   * otherwise.
   */
  def assume(f : Boolean) : Unit =
    withNDSearch(s => s.assume(f))

  /**
   * Assume that the given option is not empty, and return its contents.
   */
  def assumeIsDefined[Data](v : Option[Data]) : Data =
    withNDSearch(s => s.assumeIsDefined(v))

  /**
   * Program execution has succeeded.
   */
  def success[Result](r : Result) : Unit =
    withNDSearch(s => s.success(r))

  /**
   * Program execution has reached a dead end.
   */
  def sorry : Unit =
    withNDSearch(s => s.sorry)

  // Some alternative function names.

  def wishFor(f : Boolean) : Unit = 
    withNDSearch(s => s.wishFor(f))

  def abort : Unit =
    withNDSearch(s => s.abort)
  def failure : Unit =
    withNDSearch(s => s.failure)

}

/**
 * Trait defining a particular non-deterministic execution semantics.
 */
trait NDSearch {

  import nondet.ValueEnum

  /**
   * Start a non-deterministic computation.
   */
  def btSearch[Result](comp : => Unit) : Option[Result]

  /**
   * Start a non-deterministic computation.
   */
  def search[Result](comp : => Result) : Option[Result]

  /**
   * Non-deterministically choose a value of type <code>T</code> and continue
   * program execution.
   */
  def choose[T,R](comp : T => R)(implicit venum : ValueEnum[T]) : R

  /**
   * Non-deterministically choose a value in the given integer range and
   * continue program execution.
   */
  def chooseInt[R](r : Range)(comp : Int => R) : R

  /**
   * Mark that computation at this point can take <code>n</code> alternative
   * paths. <code>comp</comp> will usually be written as a <code>match</code>
   * block.
   */
  def alternatives[R](n : Int)(comp : Int => R) : R =
    chooseInt(0 until n)(comp)

  /**
   * Assume that the given condition is true, block program execution
   * otherwise.
   */
  def assume(f : Boolean) : Unit

  /**
   * Assume that the given option is not empty, and return its contents.
   */
  def assumeIsDefined[Data](v : Option[Data]) : Data = {
    assume(v.isDefined)
    v.get
  }

  /**
   * Program execution has succeeded.
   */
  def success[Result](r : Result) : Unit

  /**
   * Program execution has reached a dead end.
   */
  def sorry : Unit = assume(false)

  // Some alternative function names.

  def wishFor(f : Boolean) : Unit = assume(f)
  def abort : Unit   = sorry
  def failure : Unit = sorry

}

/**
 * Functions controlling non-deterministic execution with the help of
 * back-tracking.
 */
class BacktrackingSearch extends NDSearch {

  import nondet.ValueEnum

  private object BacktrackingException                    extends Exception
  private case class SuccessException[Result](r : Result) extends Exception

  def btSearch[Result](comp : => Unit) : Option[Result] =
    try {
      comp
      None
    } catch {
      case SuccessException(result : Result) =>
        Some(result)
      case BacktrackingException =>
        None
    }

  def search[Result](comp : => Result) : Option[Result] =
    throw new UnsupportedOperationException

  def success[Result](r : Result) : Unit =
    throw new SuccessException (r)

  def choose[T,R](comp : T => R)(implicit venum : ValueEnum[T]) : R =
    chooseFromIterator(venum.iterator, comp)

  def chooseInt[R](r : Range)(comp : Int => R) : R =
    chooseFromIterator(r.iterator, comp)

  private def chooseFromIterator[T,R](it : Iterator[T], comp : T => R) : R = {
    var next = it.next

    while (it.hasNext)
      try {
        val n = next
        next = it.next
        comp(n)
      } catch {
        case BacktrackingException => // use next value
      }

    // last possible value
    comp(next)
    throw new Exception(
      "with btSearch, every choice has to end with \"success\" or abort")
  }

  def assume(f : Boolean) : Unit =
    if (!f)
      throw BacktrackingException

}

object TracingSearch {

  class ExecutionTree {
    var children         : List[(Any, ExecutionTree)] = List()
    var remainingChoices : Iterator[Any]              = null
    var deadend          : Boolean                    = false

    def hasMoreChoices =
      remainingChoices.hasNext

    def getNextChoice : Any = {
      val c = remainingChoices.next
      children = (c, new ExecutionTree) :: children
      c
    }

    def getChild(c : Any) : ExecutionTree =
      children.find(p => p._1 == c).get._2

    def makeDeadend : Unit = {
      deadend = true
      children = List()
    }

    override def toString =
      "InnerNode(" + children.mkString("; ") + ")"
  }

  def findNewTrace(node   : ExecutionTree,
                   suffix : List[Any]) : Option[List[Any]] = {
    if (node.deadend) {
      None
    } else if (node.remainingChoices == null) {
      Some(suffix)
    } else if (node.hasMoreChoices) {
      Some(node.getNextChoice :: suffix)
    } else {
      var res : Option[List[Any]] = None
      for ((c, n) <- node.children)
        if (res.isEmpty) {
          res = findNewTrace(n, c :: suffix)
          if (res.isEmpty)
            n.makeDeadend
        }
      res
    }
  }

}

/**
 * Functions controlling non-deterministic execution by recording all
 * decisions made during an execution.
 */
class TracingSearch extends NDSearch {

  import nondet.ValueEnum
  import TracingSearch._

  private object FailureException extends Exception

  private var executionTree   : ExecutionTree = new ExecutionTree
  private var currentTreeNode : ExecutionTree = executionTree
  private var nextChoices     : List[Any]     = List()

  private var bound = 0

  def btSearch[Result](comp : => Unit) : Option[Result] =
    throw new UnsupportedOperationException

  def search[Result](comp : => Result) : Option[Result] = {
    var res : Option[Result] = None

    while (res.isEmpty) {
      findNewTrace(executionTree, List()) match {
        case Some(trace) =>
          nextChoices = trace.reverse
        case None =>
          return None
      }

      try {
        currentTreeNode = executionTree
        bound = 0
        res = Some(comp)
      } catch {
        case FailureException => // continue
      }
    }

    res
  }

  def success[Result](r : Result) : Unit = ???

  def choose[T,R](comp : T => R)(implicit venum : ValueEnum[T]) : R =
    chooseFromIterator(venum.iterator, comp)

  def chooseInt[R](r : Range)(comp : Int => R) : R =
    chooseFromIterator(r.iterator, comp)

  private def chooseFromIterator[T,R](it : Iterator[T], comp : T => R) : R = {
    bound = bound + 1
    if (bound > 10) {
      currentTreeNode.makeDeadend
      throw FailureException
    }

    val choice =
      nextChoices match {
        case List() => {
          assert(currentTreeNode.remainingChoices == null)
          assert(!currentTreeNode.deadend)
          currentTreeNode.remainingChoices = it
          currentTreeNode.getNextChoice
        }
        case c :: otherChoices => {
          nextChoices = otherChoices
          c
        }
      }

    currentTreeNode = currentTreeNode.getChild(choice)
    comp(choice.asInstanceOf[T])
  }

  def assume(f : Boolean) : Unit =
    if (!f) {
      currentTreeNode.makeDeadend
      throw FailureException
    }

}
