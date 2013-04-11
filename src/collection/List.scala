/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Linked List http://en.wikipedia.org/wiki/Linked_list
 *
 * Prepend - O(1)
 * Append - O(n)
 * Head - O(1)
 * Tail - O(1)
 * Lookup - O(n)
 *
 */

abstract class List[+A] {

  /**
   * The head of this list.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def head: A

  /**
   * The tail of this list.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def tail: List[A]

  def isEmpty: Boolean

  /**
   * Appends the element 'x' to this list.
   * 
   * Time - O(n)
   * Space - O(n)
   */ 
  def append[B >: A](x: B): List[B] =
    if (isEmpty) new Cons(x) 
    else new Cons(head, tail.append(x))

  /**
   * Prepends the element 'x' to this list. 
   *
   * Time - O(1)
   * Space - O(1)
   */
  def prepend[B >: A](x: B): List[B] = 
    new Cons(x, this)

  /**
   * Removes the element 'x' from the list.
   * 
   * Time - O(n)
   * Space - O(n)
   */
  def remove[B >: A](x: B): List[B] = 
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this list.")
    else if (x != head) new Cons(head, tail.remove(x))
    else tail

  /**
   * Searches for the n-th element of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def apply(n: Int): A =
    if (isEmpty) throw new NoSuchElementException("Index out of the bounds.")
    else if (n == 0) head
    else tail(n - 1)

  /**
   * Checks whether this list contains element 'x' or not.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def contains[B >: A](x: B): Boolean = 
    if (isEmpty) false
    else if (x != head) tail.contains(x)
    else true

  /**
   * Applies the 'f' function to the each element of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def foreach(f: (A) => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  /**
   * Combines all elements of this list into value.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def fold[B](n: B)(op: (B, A) => B): B = {
    def loop(l: List[A], a: B): B =
      if (l.isEmpty) a
      else loop(l.tail, op(a, l.head))

    loop(this, n)
  }

  /**
   * Creates new list by mapping this list to the 'f' function.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def map[B >: A](f: (A) => B): List[B] = 
    if (isEmpty) this
    else tail.map(f).prepend(f(head))

  /**
   * Caclulates the sum of all elements of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  /**
   * Calculates the product of all elements of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

  /**
   * Searches for the minimal element of this list.
   * 
   * Time - O(n)
   * Space - O(n)
   */ 
  def min[B >: A](implicit ordering: Ordering[B]): B = 
    if (isEmpty) throw new NoSuchElementException("Nill.min")
    else if (tail.isEmpty) head
    else ordering.min(head, tail.min(ordering))

  /**
   * Searches for the maximal element of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def max[B >: A](implicit ordering: Ordering[B]): B = 
    if (isEmpty) throw new NoSuchElementException("Nill.max")
    else if (tail.isEmpty) head
    else ordering.max(head, tail.max(ordering))

  /**
   * Reverses this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def reverse: List[A] = { 
    def loop(s: List[A], d: List[A]): List[A] = 
      if (s.isEmpty) d
      else loop(s.tail, d.prepend(s.head))

    loop(this, Nill)
  }

  /**
   * Calculates the length of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def length: Int = 
    if (isEmpty) 0 
    else 1 + tail.length

  /**
   * Converts this list into the string representation.
   * 
   * Time - O(n)
   * Space - O(n)
   */
  override def toString: String = {
    def loop(h: A, t: List[A], s: String): String = 
      if (!t.isEmpty) loop(t.head, t.tail, s + h + ", ")
      else s + h

    if (isEmpty) "List[]"
    else "List[" + loop(head, tail, "") + "]"
  }
}

object Nill extends List[Nothing] { // since 'Nil' already reserved 
  def head: Nothing = throw new NoSuchElementException("Nill.head")
  def tail: List[Nothing] = throw new NoSuchElementException("Nill.tail") 

  def isEmpty: Boolean = true
}

class Cons[A](h: A, t: List[A] = Nill) extends List[A] {
  def head: A = h
  def tail: List[A] = t

  def isEmpty: Boolean = false
}

object List {

  /**
   * Creates a new list from the 'xs' sequence.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def apply[A](xs: A*): List[A] = {
    var r: List[A] = Nill
    for (x <- xs.reverse) r = r.prepend(x)
    r
  }
}
