/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Stack http://en.wikipedia.org/wiki/Stack_(abstract_data_type)
 *
 * Push - O(1)
 * Top - O(1)
 * Pop - O(1)

 */

abstract class Stack[+A] {
  def top: A
  def rest: Stack[A]

  def isEmpty: Boolean

  def pop: (A, Stack[A]) = (top, rest)

  def push[B >: A](v: B): Stack[B] = new NonEmptyStack(v, this)
}

object NIL extends Stack[Nothing] { // since 'Nil' already reserved 
  def top: Nothing = throw new NoSuchElementException("Empty stack.")
  def rest: Nothing = throw new NoSuchElementException("Empty stack.")

  def isEmpty: Boolean = true
}

class NonEmptyStack[A](t: A, r: Stack[A] = NIL) extends Stack[A] {
  def top: A = t
  def rest: Stack[A] = r

  def isEmpty: Boolean = false
}
