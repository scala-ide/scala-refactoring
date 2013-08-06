/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

/**
 * Transformations is the basis for all refactoring transformations.
 *
 * A transformation is a Function from X ⇒ Option[X], and can be
 * combined with other transformations in two ways:
 *   andThen - which applies the second transformation only if the
 *             first one was successful, i.e. returned Some(_).
 *   orElse  - which is applied only when the first transformation
 *             returned None.
 *
 * Xs are typically instances of global.Tree, but this is not
 * enforced. Once a transformations is assembled, it has to be applied
 * to a tree and its children. The function `all` applies a transformation
 * to the children of a tree. In the case of the trees, the tree has to
 * apply the transformation to all children and return one single tree.
 *
 * Additional functions are provided that apply a transformation top-down
 * or bottom-up.
 */
trait Transformations {

  this: common.CompilerAccess =>

  type X = global.Tree

  def traverse(x: X, f: X ⇒ X): X

  abstract class Transformation[X, Y] extends (X ⇒ Option[Y]) {
    self ⇒

    def apply(x: X): Option[Y]

    def andThen[Z](t: ⇒ T[Y, Z]) = new T[X, Z] {
      def apply(x: X): Option[Z] = {
        self(x) flatMap t
      }
    }
    def &>[Z](t: ⇒ T[Y, Z]) = andThen(t)

    def orElse(t: ⇒ T[X, Y]) = new T[X, Y] {
      def apply(x: X): Option[Y] = {
        self(x) orElse t(x)
      }
    }
    def |>(t: ⇒ T[X, Y]) = orElse(t)
  }

  private type T[X, Y] = Transformation[X, Y]

  /**
   * Construct a transformation from a partial function; this is the
   * most commonly used way to create new transformations, for example
   * like:
   *
   *   val reverse_all_class_members = transformation[Tree, Tree] {
   *     case t: Template => t.copy(body = t.body.reverse)
   *   }
   */
  def transformation[X, Y](f: PartialFunction[X, Y]) = new T[X, Y] {
    def apply(x: X): Option[Y] = f lift x
  }

  /**
   * We often want to use transformations as predicates, which execute
   * the next transformations if the result is true. For example:
   *
   *   val tree_with_range_pos = filter[Tree] {
   *     case t: Tree => t.pos.isRange
   *   }
   *
   * We can then use the predicate like this:
   *   tree_with_range_pos andThen do_something_with_the_tree orElse nothing
   */
  def predicate[X](f: PartialFunction[X, Boolean]) = new T[X, X] {
    def apply(t: X): Option[X] = if (f.isDefinedAt(t) && f(t)) Some(t) else None
  }

  def predicate[X](f: X ⇒ Boolean) = new T[X, X] {
    def apply(t: X): Option[X] = if (f(t)) Some(t) else None
  }

  /**
   * Always succeeds and returns the input unchanged.
   */
  def succeed[X] = new T[X, X] {
    def apply(in: X): Option[X] = Some(in)
  }

  def id[X] = succeed[X]

  /**
   * Always fails, independent of the input.
   */
  def fail[X] = new T[X, X] {
    def apply(in: X): Option[X] = None
  }

  def not[X](t: => T[X, X]) = new Transformation[X, X] {
    def apply(x: X) = if (t(x).isDefined) None else Some(x)
  }
  def !  [X](t: ⇒ T[X, X]) = not(t)

  /**
   * Applies a transformation to all subtrees of a tree T,
   * returning a new tree,typically of the same kind as T.
   *
   * If the transformation fails on one child, abort and
   * fail the whole application.
   */
  def allChildren(t: ⇒ T[X, X]) = new T[X, X] {
    def apply(in: X): Option[X] = {
      Some(traverse(in, child => t(child) getOrElse (return None)))
    }
  }

  /**
   * Applies a transformation to all subtrees of a tree T,
   * returning a new tree,typically of the same kind as T.
   *
   * If the transformation fails on one child, apply the
   * identity transformation `id` and don't fail, unlike
   * `allChildren`.
   */
  def matchingChildren(t: T[X, X]) = allChildren(t |> id[X])

  /**
   * Applies a transformation top-down, that is, it applies
   * the transformation to the tree T and then passes the
   * transformed T to all children. The consequence is that
   * the children "see" their new parent.
   */
  def ↓       (t: ⇒ T[X, X]): T[X, X] = t &> allChildren(↓(t))
  def topdown (t: ⇒ T[X, X]): T[X, X] = t &> allChildren(↓(t))
  def preorder(t: ⇒ T[X, X]): T[X, X] = t &> allChildren(↓(t))

  /**
   * Applies a transformation bottom-up, that is, it applies
   * the transformation to the children of the tree first and
   * then to their parent. The consequence is that the parent
   * "sees" its transformed children.
   */
  def ↑        (t: ⇒ T[X, X]): T[X, X] = allChildren(↑(t)) &> t
  def bottomup (t: ⇒ T[X, X]): T[X, X] = allChildren(↑(t)) &> t
  def postorder(t: ⇒ T[X, X]): T[X, X] = allChildren(↑(t)) &> t

  /**
   * Do a transformation until it succeeded once, then just fail.
   *
   * Note that because of the statefulness of once, you need to
   * make sure that it isn't accidentally passed as a by-name
   * parameter to another transformation and instantiated multiple
   * times.
   */
  def once [X <: AnyRef](t: T[X, X]): T[X, X] = new T[X, X] {
    var alreadyMatched = false
    def apply(x: X): Option[X] = {
      if(alreadyMatched) return None
      t(x) map { res =>
        // returning the same reference
        // does not count as "matched"
        alreadyMatched = !(x eq res)
        res
      }
    }
  }

  def traverseAndTransformAll (t: ⇒ T[X, X]): T[X, X] = t |> topdown(matchingChildren(t))

  /**
   * Creates a transformation that always returns the value x.
   */
  def constant(y: X) = transformation[X, X] {
    case _ => y
  }
}
