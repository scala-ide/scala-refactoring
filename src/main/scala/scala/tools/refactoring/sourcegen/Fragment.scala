/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

trait Fragment {
  self =>
  val leading:  Layout
  val center:   Layout
  val trailing: Layout

  val pre  = NoRequisite: Requisite
  val post = NoRequisite: Requisite

  override def toString() = asText

  def dropLeadingLayout: Fragment = new Fragment {
    val leading  = NoLayout
    val center   = self.center
    val trailing = self.trailing
    override val pre  = self.pre
    override val post = self.post
  }

  def dropLeadingIndentation: Fragment = new Fragment {
    val leading  = Layout(self.leading.asText.replaceFirst("""^\s*""", ""))
    val center   = self.center
    val trailing = self.trailing
    override val pre  = self.pre
    override val post = self.post
  }

  def dropTrailingLayout: Fragment = new Fragment {
    val leading  = self.leading
    val center   = self.center
    val trailing = NoLayout
    override val pre  = self.pre
    override val post = NoRequisite
  }

  def isEmpty: Boolean = this match {
    case EmptyFragment => true
    case _ if asText == "" => true
    case _ => false
  }

  def ifNotEmpty(f: Fragment => Fragment): Fragment = this match {
    case EmptyFragment => EmptyFragment
    case _ if asText == "" => EmptyFragment
    case _ => f(this)
  }

  def toLayout: Layout = new Layout {
    def asText = self.asText
  }

  def asText: String = pre(NoLayout, leading).asText + center.asText + post(trailing, NoLayout).asText

  /**
   * Combines two fragments, makes sure that
   * Requisites are satisfied.
   *
   * Combining two fragments (a,b,c) and (d,e,f)
   * yields a fragment (a,bcde,f).
   */
  def ++ (o: Fragment): Fragment = o match {
    case EmptyFragment => this
    case _ => new Fragment {
      val leading  = self.leading
      val center   = self.center ++  (self.post ++ o.pre)(self.trailing, o.leading) ++ o.center
      val trailing = o.trailing

      override val pre  = self.pre
      override val post = o.post
    }
  }

  /**
   * Combines a fragment with a layout, makes sure that
   * Requisites are satisfied.
   *
   * Combining (a,b,c) and (d)
   * yields a fragment (a,b,cd).
   */
  def ++ (o: Layout): Fragment = o match {
    case NoLayout => this
    case _ => new Fragment {
      val leading  = self.leading
      val center   = self.center
      val trailing = self.post(self.trailing, o)

      override val pre  = self.pre
      override val post = /*if (self.post.isRequired(this.trailing, NoLayout)) self.post else*/ NoRequisite
    }
  }

  def ++ (after: Requisite, before: Requisite = NoRequisite): Fragment = {
    new Fragment {
      val leading  = self.leading
      val center   = self.center
      val trailing = self.trailing

      override val pre  = before ++ self.pre
      override val post = self.post ++ after
    }
  }
}

abstract class EmptyFragment extends Fragment {
  val leading  = NoLayout: Layout
  val center   = NoLayout: Layout
  val trailing = NoLayout: Layout
}

object EmptyFragment extends EmptyFragment

object Fragment {

  def unapply(f: Fragment) = Some((f.leading, f.center, f.trailing))

  def apply(l: Layout, c: Layout, t: Layout) = new Fragment {
    val leading = l
    val center = c
    val trailing = t
  }

  def apply(s: String) = new EmptyFragment {
    override val center = Layout(s)
  }
}
