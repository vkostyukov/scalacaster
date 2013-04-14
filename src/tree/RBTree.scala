/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Red-Black Tree http://en.wikipedia.org/wiki/Red-black_tree
 *
 * Insert - O(log n)
 * Lookup - O(log n)  
 * Remove - O(log n)
 */

abstract class RBTree[+A <% Ordered[A]] {

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Checks whether this tree is black or not.
   */
  def isBlack: Boolean 

  /**
   * Checks whether this tree is red or not.
   */
  def isRed: Boolean = !isBlack

  /**
   * Adds given element 'x' into this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def add[B >: A <% Ordered[B]](x: B): RBTree[B] = ???

  /**
   * Removes given element 'x' from this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def remove[B >: A <% Ordered[B]](x: B): RBTree[B] = ???

  /**
   * Checks whether this tree contans element 'x' or not.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def contains[B >: A <% Ordered[B]](x: B): Boolean = ???
}

class RedNode[A] extends RBTree[A] {

  def isEmpty = false
  def isBlack = false

}

class BlackNode[A] extends RBTree[A] {

  def isEmpty = false
  def isBlack = true
}

class Leaf extends RBTree[Nothing] {

  def isEmpty = true
  def isBlack = true

}
