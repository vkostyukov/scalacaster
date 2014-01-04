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

class Stack[+A](self: List[A]) {

  /**
   * The top of this stack.
   */
  def top: A = self.head

  /**
   * The rest of this stack.
   */
  def rest: Stack[A] = new Stack(self.tail)

  /**
   * Checks whether this stack is empty or not.
   */
  def isEmpty: Boolean = self.isEmpty

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
  def push[B >: A](x: B): Stack[B] = new Stack(x :: self)
}

object Stack {

   /**
    * Returns an empty stack instance.
    *
    * Time - O(1)
    * Space - O(1)
    */
   def empty[A]: Stack[A] = new Stack(Nil)

   /**
    * Creates a new stack from given 'xs' sequence.
    *
    * Time - O(n)
    * Space - O(1)
    */
   def apply[A](xs: A*): Stack[A] = {
    var r: Stack[A] = Stack.empty
    for (x <- xs) r = r.push(x)
    r
  }
}
