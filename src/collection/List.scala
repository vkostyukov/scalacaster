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
  def head: A
  def tail: List[A]

  def +>[B >: A](v: B): List[B] = // append
    if (isEmpty) new Cons(v) 
    else new Cons(head, tail +> v)

  def <+[B >: A](v: B): List[B] = // prepend
    new Cons(v, this)

  def apply(i: Int): A = { // lookup
    var n = 0
    var these = this
    while (n < i && !these.isEmpty) {
      n += 1
      these = these.tail
    }
    if (n != i)
      throw new NoSuchElementException("Index out of the bounds.")
    else these.head
  }

  def isEmpty: Boolean

  def reverse: List[A] = { // brief example of tail recursion
    def loop(s: List[A], d: List[A]): List[A] = 
      if (s.isEmpty) d
      else loop(s.tail, d <+ head)

    loop(this, Nill)
  }
}

object Nill extends List[Nothing] { // since 'Nill' already reserved 
  def head: Nothing = throw new NoSuchElementException("Empty list.")
  def tail: List[Nothing] = throw new NoSuchElementException("Empty list.") 

  def isEmpty: Boolean = true
}

class Cons[A](h: A, t: List[A] = Nill) extends List[A] {
  def head: A = h
  def tail: List[A] = t

  def isEmpty: Boolean = false
}

object List {
  def apply[B](vs: B*): List[B] = {
    var r: List[B] = Nill
    for (v <- vs.reverse) r = r <+ v
    r
  }
}
