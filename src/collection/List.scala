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

  def append[B >: A](x: B): List[B] =
    if (isEmpty) new Cons(v) 
    else new Cons(head, tail.append(x))

  def prepend[B >: A](x: B): List[B] = 
    new Cons(x, this)

  def apply(n: Int): A = {
    def loop(l: List[A], m: Int): List[A] = 
      if (l.isEmpty) throw new NoSuchElementException("Index out of the bounds.")
      else if (m == 0) l
      else loop(l.tail, m - 1)

    loop(this, n)
  }

  def isEmpty: Boolean

  def reverse: List[A] = { 
    def loop(s: List[A], d: List[A]): List[A] = 
      if (s.isEmpty) d
      else loop(s.tail, d.prepend(head))

    loop(this, Nill)
  }

  def length: Int = 
    if (isEmpty) 0 
    else 1 + tail.length
}

object Nill extends List[Nothing] { // since 'Nil' already reserved 
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
  def apply[A](xs: A*): List[A] = {
    var r: List[A] = Nill
    for (x <- xs.reverse) r = r <+ x
    r
  }
}
