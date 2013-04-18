/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Binary Search Tree http://en.wikipedia.org/wiki/Binary_search_tree
 *
 * Insert - O(log n)
 * Lookup - O(log n)  
 * Remove - O(log n)
 *
 * -Notes-
 * 
 * This is an efficient implementation of ordinary binary search tree with small modification.
 * Sized BST garantees O(log n) running time for ordering operations such as 'nthMax', 'nthMin'
 * and 'rank'. The main idea of sized BST - is to store one more addtional field in each node -
 * the size of tree that rooted in that node.
 */

abstract class SizedTree[+A <% Ordered[A]] {

  /**
   * The value of this tree.
   */
  def value: A

  /**
   * The left child of this tree.
   */
  def left: SizedTree[A]

  /**
   * The right child of this tree.
   */
  def right: SizedTree[A]

  /**
   * The size child of this tree.
   */
  def size: Int

  /**
   * Checks whether this tree empty or not.
   */
  def isEmpty: Boolean

  /**
   * Adds given element 'x' into this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def add[B >: A <% Ordered[B]](x: B): SizedTree[B] = 
    if (isEmpty) SizedTree(x)
    else if (x < value) SizedTree(value, left.add(x), right)
    else if (x > value) SizedTree(value, left, right.add(x))
    else this

  /**
   * Removes given element 'x' from this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def remove[B >: A <% Ordered[B]](x : B): SizedTree[B] = 
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) SizedTree(value, left.remove(x), right)
    else if (x > value) SizedTree(value, left, right.remove(x))
    else if (left.isEmpty && right.isEmpty) SizedTree.empty
    else if (left.isEmpty) right
    else if (right.isEmpty) left
    else {
      val succ = right.min
      SizedTree(succ, left, right.remove(succ))
    }

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

  def nthMin(n: Int): A = apply(n)
  def nthMax(n: Int): A = apply(size - n - 1)

  /**
   * Searches for the 'n'-th ordered statistic of this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def apply(n: Int): A = 
    if (isEmpty) throw new NoSuchElementException("Index " + n + " out of the bounds.")
    else if (left.size < n) left(n)
    else if (left.size > n) right(left.size - n - 1)
    else value

  /**
   * Calculates the number of elements that less or equal to given 'x'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def rank[B >: A <% Ordered[B]](x: B): Int =
    if (isEmpty) 0
    else if (x < value) left.rank(x)
    else if (x > value) 1 + left.size + right.rank(x)
    else left.size
}

object Leaf extends SizedTree[Nothing] {
  def value: Nothing = throw new NoSuchElementException("Leaf.value")
  def left: SizedTree[Nothing] = throw new NoSuchElementException("Leaf.left")
  def right: SizedTree[Nothing] = throw new NoSuchElementException("Leaf.right")
  def size: Int = 0

  def isEmpty: Boolean = true
}

class Node[A <% Ordered[A]](v: A, l: SizedTree[A], r: SizedTree[A], s: Int) extends SizedTree[A] {
  def value: A = v
  def left: SizedTree[A] = l
  def right: SizedTree[A] = r
  def size: Int = s

  def isEmpty: Boolean = false
}

object SizedTree {

  /**
   * An empty tree singleton instance.
   */
  def empty[A]: SizedTree[A] = Leaf

  /**
   * Creates a new tree with given value 'v' and children 'l' and 'r'.
   * 
   * Time - O(1)
   * Space - O(1)
   */
  def apply[A <% Ordered[A]](x: A, l: SizedTree[A] = Leaf, r: SizedTree[A] = Leaf): SizedTree[A] = 
    new Node(x, l, r, l.size + r.size + 1)

  /**
   * Builds a new balanced tree from given sorted array 'a'.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fromSortedArray[A <% Ordered[A]](a: Array[A]): SizedTree[A] = {
    def loop(l: Int, r: Int): SizedTree[A] = 
      if (l == r) SizedTree.empty
      else {
        val p = (l + r) / 2
        SizedTree(a(p), loop(l, p), loop(p + 1, r))
      }

    loop(0, a.length)
  }
}

