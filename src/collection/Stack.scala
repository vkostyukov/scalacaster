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
  /**
   * The top of this stack.
   */
  def top: A

  /**
   * The rest of this stack.
   */
  def rest: Stack[A]

  /**
   * Checks whether this stack is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Pops top element from this stack.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def pop: (A, Stack[A]) = (top, rest)

  /**
   * Pushes given element 'x' into this stack.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def push[B >: A](x: B): Stack[B] = new NonEmptyStack(x, this)
}

object EmptyStack extends Stack[Nothing] {
  def top: Nothing = throw new NoSuchElementException("EmptyStack.top")
  def rest: Nothing = throw new NoSuchElementException("EmptyStack.rest")

  def isEmpty: Boolean = true
}

class NonEmptyStack[A](t: A, r: Stack[A] = EmptyStack) extends Stack[A] {
  def top: A = t
  def rest: Stack[A] = r

  def isEmpty: Boolean = false
}

object Stack {

   /**
    * Returns an empty stack instance.
    *
    * Time - O(1)
    * Space - O(1)
    */
   def empty[A]: Stack[A] = EmptyStack

   /**
    * Creates a new stack from given 'xs' sequence.
    *
    * Time - O(n)
    * Space - O(1)
    */
   def apply[A](xs: A*): Stack[A] = {
    var r: Stack[A] = EmptyStack
    for (x <- xs) r = r.push(x)
    r
  }
}
