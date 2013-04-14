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
   * The value of this tree.
   */
  def value: A

  /**
   * The left child of this tree.
   */
  def left: RBTree[A]

  /**
   * The right child of this tree.
   */
  def right: RBTree[A]

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
  def add[B >: A <% Ordered[B]](x: B): RBTree[B] = balancedAdd(x).blacken

  /**
   * Removes given element 'x' from this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def remove[B >: A <% Ordered[B]](x: B): RBTree[B] = balancedRemove(x).blacken

  /**
   * Checks whether this tree contans element 'x' or not.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def contains[B >: A <% Ordered[B]](x: B): Boolean = 
    if (isEmpty) false
    else if (x < value) left.contains(x)
    else if (x > value) right.contains(x)
    else true

  /**
   * Balanced version of "add" method.
   *
   * Time - O(???)
   * Space - O(???)
   */
  private def balancedAdd[B >: A <% Ordered[B]](x: B): RBTree[B] = 
    if (isEmpty) new RedNode(x)
    else if (x < value) balanceLeft(this, left.add(x), right)
    else if (x > value) balanceRight(this, left, right.add(x))
    else this

  /**
   * Balanced version of "remove" method.
   *
   * Time - O(???)
   * Space - O(???)
   */
  private def balancedRemove[B >: A <% Ordered[B]](x: B): RBTree[B] = ???

  /**
   * Performs the left rotation of given tree 't'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private def balanceLeft[B >: A <% Ordered[B]](t: RBTree[B], l: RBTree[B], r: RBTree[B]): RBTree[B] = ???

  /**
   * Performs the rigth rotation of given tree 't'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private def balanceRight[B >: A <% Ordered[B]](t: RBTree[B], l: RBTree[B], r: RBTree[B]): RBTree[B] = ???

  /**
   * Converts this node into black one.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private def blacken: RBTree[A] = 
    if (isBlack) this
    else new BlackNode(value, left, right)
}

class RedNode[A <% Ordered[A]](v: A, l: RBTree[A] = Leaf, r: RBTree[A] = Leaf) extends RBTree[A] {
  def value: A = v
  def left: RBTree[A] = l
  def right: RBTree[A] = r

  def isEmpty = false
  def isBlack = false
}

class BlackNode[A <% Ordered[A]](v: A, l: RBTree[A] = Leaf, r: RBTree[A] = Leaf) extends RBTree[A] {
  def value: A = v
  def left: RBTree[A] = l
  def right: RBTree[A] = r

  def isEmpty = false
  def isBlack = true
}

object Leaf extends RBTree[Nothing] {
  def value: Nothing = throw new NoSuchElementException("Leaf.value")
  def left: RBTree[Nothing] = throw new NoSuchElementException("Leaf.left")
  def right: RBTree[Nothing] = throw new NoSuchElementException("Leaf.right")

  def isEmpty = true
  def isBlack = true
}

object RBTree {

  /**
   * Creates a new red-black tree from given 'xs' sequence.
   *
   * Time - O(n log n)
   * Space - O(log n)
   */
  def apply[A <% Ordered[A]](xs: A*): RBTree[A] = {
    var r: RBTree[A] = Leaf
    for (x <- xs) r = r.add(x)
    r
  }
}
