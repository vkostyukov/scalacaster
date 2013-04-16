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
   * Returns the sumbtree of this tree with root element 'x'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def subtree[B >: A <% Ordered[B]](x: B): Tree[B] =
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) left.subtree(x)
    else if (x > value) right.subtree(x)
    else this

    /**
   * Searches for the minimal element of this tree.
   * 
   * Time - O(log n)
   * Space - O(log n)
   */
  def min: A = 
    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (left.isEmpty) value
    else left.min

  /**
   * Searches for the maximal element of this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def max: A = 
    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (right.isEmpty) value
    else right.max

  /**
   * Calculates the height of this tree.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def height: Int =
    if (isEmpty) 0
    else 1 + math.max(left.height, right.height)

  /**
   * Calculates the depth for given element 'x'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def depth[B >: A <% Ordered[B]](x: B): Int =
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) 1 + left.depth(x)
    else if (x > value) 1 + right.depth(x)
    else 0

  /**
   * Balanced version of "add" method.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  private def balancedAdd[B >: A <% Ordered[B]](x: B): RBTree[B] = 
    if (isEmpty) RedTree(x)
    else if (x < value) balance(this, left.balancedAdd(x), right)
    else if (x > value) balance(this, left, right.balancedAdd(x))
    else this

  /**
   * Balanced version of "remove" method.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  private def balancedRemove[B >: A <% Ordered[B]](x: B): RBTree[B] = ???

  /**
   * Performs the balancing of given tree 't'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private def balance[B >: A <% Ordered[B]](t: RBTree[B], l: RBTree[B], r: RBTree[B]): RBTree[B] =
    if (!t.isEmpty && t.isBlack)
      if (!l.isEmpty && l.isRed)
        if (!l.left.isEmpty && l.left.isRed) 
          RedTree(l.value, BlackTree(l.left.value, l.left.left, l.left.right), BlackTree(t.value, l.right, r))
        else if (!l.right.isEmpty && l.right.isRed) 
          RedTree(l.right.value, BlackTree(l.value, l.left, l.right.left), BlackTree(t.value, l.right.right, r))
        else mkTree(t, l, r)
      else if (!r.isEmpty && r.isRed)
        if (!r.left.isEmpty && r.left.isRed) 
          RedTree(r.left.value, BlackTree(t.value, l, r.left.left), BlackTree(r.value, r.left.right, r.right))
        else if (!r.right.isEmpty && r.right.isRed) 
          RedTree(r.value, BlackTree(t.value, l, r.left), BlackTree(r.right.value, r.right.left, r.right.right))
        else mkTree(t, l, r)
      else mkTree(t, l, r)
    else mkTree(t, l, r)

  /**
   * Converts this node into black one.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private def blacken: RBTree[A] = 
    if (isBlack) this
    else BlackTree(value, left, right)

  /**
   * Copies a given tree 't' and children 'l' and 'r' into new node.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private def mkTree[B >: A <% Ordered[B]](t: RBTree[B], l: RBTree[B], r: RBTree[B]): RBTree[B] = 
    if (t.isBlack) BlackTree(t.value, l, r)
    else RedTree(t.value, l, r)

  /**
   * Convertes this tree into the string representation.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  override def toString(): String = 
    if (isEmpty) "."
    else if (isRed) "r{" + left + value + right + "}"
    else "b{" + left + value + right + "}"
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
   * Returns an empty red-black tree instance.
   *
   * Time - O(1)
   * Space - O(1)
   */
   def empty[A]: RBTree[A] = Leaf

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

object RedTree {

  /**
   * Creates a new red-tree with given element 'x' and children 'l' and 'r'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def apply[A <% Ordered[A]](x: A, l: RBTree[A] = Leaf, r: RBTree[A] = Leaf): RBTree[A] = new RedNode(x, l, r)
}

object BlackTree {

  /**
   * Creates a new black-tree with given element 'x' and children 'l' and 'r'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def apply[A <% Ordered[A]](x: A, l: RBTree[A] = Leaf, r: RBTree[A] = Leaf): RBTree[A] = new BlackNode(x, l, r)
}
