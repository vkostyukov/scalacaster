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
 * This is an efficient implementation of binary search tree. This tree garantees
 * O(log n) running time for ordered operations like 'nthMin', 'nthMax' and 'rank'.
 * The main idea here - is use additional node field that stores size of tree rotted
 * at this node. This allows to get the size of tree in O(1) instead of linear time.
 */

abstract class Tree[+A <% Ordered[A]] {

  /**
   * The value of this tree.
   */
  def value: A

  /**
   * The left child of this tree.
   */
  def left: Tree[A]

  /**
   * The right child of this tree.
   */
  def right: Tree[A]

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

  /**
   * The size of this tree.
   */
  def size: Int

  /**
   * Checks whether this tree is a binary search tree or not.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def isValid: Boolean =
    if (isEmpty) true
    else if (left.isEmpty && right.isEmpty) true
    else if (left.isEmpty) right.value >= value && right.isValid
    else if (right.isEmpty) left.value <= value && left.isValid
    else left.value <= value && right.value >= value && left.isValid && right.isValid

  /**
   * Checks whether this tree is balanced or not.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def isBalanced: Boolean = {
    def loop(t: Tree[A]): Int = 
      if (t.isEmpty) 0
      else {
        val l = loop(t.left)
        if (l == -1) -1
        else {
          val r = loop(t.right)
          if (r == -1) -1
          else if (math.abs(l - r) > 1) -1
          else 1 + math.max(l, r)
        }
      }

    !(loop(this) == -1)
  }

  /**
   * Adds given element 'x' into this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def add[B >: A <% Ordered[B]](x: B): Tree[B] =
    if (isEmpty) Tree(x)
    else if (x < value) Tree(value, left.add(x), right)
    else if (x > value) Tree(value, left, right.add(x))
    else this

  /**
   * Removes given element 'x' from this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def remove[B >: A <% Ordered[B]](x: B): Tree[B] =
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) Tree(value, left.remove(x), right)
    else if (x > value) Tree(value, left, right.remove(x))
    else {
      if (left.isEmpty && right.isEmpty) Tree.empty
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else {
        val succ = right.min
        Tree(succ, left, right.remove(succ))
      }
    }

  /**
   * Checks whether this tree contains element 'x' or not.
   *
   * Time - O(log n)
   * Space - O(log n)
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
   * Checks whether the 't' tree is a subtree of this tree.
   *
   * NOTE: This task can be done in O(n + m) running time 
   * by using the following algorithm:
   *
   * 1. convert this tree into string representation using pre-order and in-order walks - O(n)
   * 2. convert other tree into string representation using pre-order and in-order walks - O(m)
   * 3. check whether second in-order string is substring of the first in-order string - O(log n)
   * 3. check whether second pre-order string is substring of the first pre-order string - O(log n)
   *
   * HINT: 'isSubstring' checking can be done with suffix-tree in O(m) but requeres O(n) time for
   *       it's building. 
   *
   * Time - O(n log n)
   * Space - O(log n)
   */
  def isSubtree[B >: A <% Ordered[B]](t: Tree[B]): Boolean = {
    def loop(a: Tree[B], b: Tree[B]): Boolean = 
      if (a.isEmpty && b.isEmpty) true
      else if (a.isEmpty || b.isEmpty) false
      else a.value == b.value && loop(a.left, b.left) && loop(a.right, b.right)

    loop(subtree(t.value), t)
  }

  /**
   * Merges this tree with given 't' tree.
   *
   * NOTE: This taks can be done in O(n + m) running time by using 
   * the following algorithm:
   *
   * 1. convert this tree into list - O(n)
   * 2. convert other tree into list - (m)
   * 3. merge these list into one - O(n + m)
   * 4. build a new tree from sorted list - O(n + m)
   *
   * Time - O(n log n)
   * Space - O(log n)
   */
  def merge[B >: A <% Ordered[B]](t: Tree[B]): Tree[B] = {
    def loop(s: Tree[B], d: Tree[B]): Tree[B] = 
      if (s.isEmpty) d
      else loop(s.right, loop(s.left, d.add(s.value)))

    loop(this, t)
  }

  /**
   * Applies the 'f' function to the each element of this tree.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def foreach(f: (A) => Unit): Unit = 
    if (!isEmpty) {
      left.foreach(f)
      f(value)
      right.foreach(f)
    }

  /**
   * Combines all elements of this tree into value.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fold[B](n: B)(op: (B, A) => B): B = {
    def loop(t: Tree[A], a: B): B =
      if (t.isEmpty) a
      else loop(t.right, op(loop(t.left, a), t.value))

    loop(this, n)
  }

  /**
   * Creates a new tree by mapping this tree to the 'f' function.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def map[B <% Ordered[B]](f: (A) => B): Tree[B] = 
    if (isEmpty) Tree.empty
    else Tree(f(value), left.map(f), right.map(f))

  /**
   * Calculates the sum of all elements of this tree.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  /**
   * Calculates the product of all elements of this list.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

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
   * Searches for the successor of given element 'x'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def successor[B >: A <% Ordered[B]](x: B): A = {
    def forward(t: Tree[A], p: List[Tree[A]]): A =
      if (t.isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
      else if (x < t.value) forward(t.left, t :: p)
      else if (x > t.value) forward(t.right, t :: p)
      else if (!t.right.isEmpty) t.right.min
      else backward(t, p)

    def backward(t: Tree[A], p: List[Tree[A]]): A = 
      if (p.isEmpty) throw new NoSuchElementException("The " + x + " doesn't have an successor.")
      else if (t == p.head.right) backward(p.head, p.tail)
      else p.head.value

    forward(this, Nil)
  }

  /**
   * Searches for the predecessor of given element 'x'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def predecessor[B >: A <% Ordered[B]](x: B): A = {
    def forward(t: Tree[A], p: List[Tree[A]]): A =
      if (t.isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
      else if (x < t.value) forward(t.left, t :: p)
      else if (x > t.value) forward(t.right, t :: p)
      else if (!t.left.isEmpty) t.left.max
      else backward(t, p)

    def backward(t: Tree[A], p: List[Tree[A]]): A = 
      if (p.isEmpty) throw new NoSuchElementException("The " + x + " doesn't have an predecessor.")
      else if (t == p.head.left) backward(p.head, p.tail)
      else p.head.value

    forward(this, Nil)
  }

  /**
   * Searches for the first common ancestor of two given elements 'x' and 'y'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def ancestor[B >: A <% Ordered[B]](x: B, y: B): A = {
    def loop(t: Tree[A]): A = 
      if (x < t.value && y < t.value) loop(t.left)
      else if (x > t.value && y > t.value) loop(t.right)
      else t.value

    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (!contains(x)) throw new NoSuchElementException("Tree doesn't contain " + x + ".")
    else if (!contains(y)) throw new NoSuchElementException("Tree doesn't contain " + y + ".")
    else loop(this)
  }

  /**
   * Searches for the lower bound element of given 'x'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def lowerBound[B >: A <% Ordered[B]](x: B): A =
    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (x < value)
      if (!left.isEmpty) left.lowerBound(x)
      else value
    else if (x > value)
      if (!right.isEmpty) { val v = right.lowerBound(x); if (v > x) value else v }
      else value
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

  /**
   * Searches for the upper bound element of given 'x'.
   *
   * Time - O(log n)
   * Time - O(log n)
   */
  def upperBound[B >: A <% Ordered[B]](x: B): A = 
    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (x < value)
      if (!left.isEmpty) { val v = left.upperBound(x); if (v < x) value else v }
      else value
    else if (x > value)
      if (!right.isEmpty) right.upperBound(x)
      else value
    else value

  /**
   * Calculates the path for given element 'x'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def path[B >: A <% Ordered[B]](x: B): List[Tree[A]] = 
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) this :: left.path(x)
    else if (x > value) this :: right.path(x)
    else List(this)

  /**
   * Calculates the trace for given element 'x'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def trace[B >: A <% Ordered[B]](x: B): List[A] = 
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) this.value :: left.trace(x)
    else if (x > value) this.value :: right.trace(x)
    else List(this.value)

  /**
   * Searches for the n-th maximum element of this tree.
   *
   * Time - O(log n)
   * Time - O(log n)
   */
  def nthMax(n: Int): A = apply(size - n - 1)

  /**
   * Searches fot the n-tn minimum element of this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def nthMin(n: Int): A = apply(n)

  /**
   * Constructs the list of 'n' largest elements of this tree.
   *
   * Note: We suppose that list.size runs in O(1).
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def takeLargest(n: Int): List[A] = {
    def loop(t: Tree[A], l: List[A]): List[A] = 
      if (t.isEmpty || l.size == n) l
      else {
        val ll = loop(t.right, l)
        if (ll.size == n) ll
        else loop(t.left, t.value :: ll)
      }

    loop(this, Nil).reverse
  }

  /**
   * Constructs the list of 'n' smallest elements of this list.
   *
   * Note: We suppose that list.size runs in O(1).
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def takeSmallest(n: Int): List[A] = {
    def loop(t: Tree[A], l: List[A]): List[A] = 
      if (t.isEmpty || l.size == n) l
      else {
        val ll = loop(t.left, l)
        if (ll.size == n) ll
        else loop(t.right, t.value :: ll)
      }

    loop(this, Nil).reverse
  }

  /**
   * Searches the longest possible leaf-to-leaf path in this tree.
   *
   * Time - O(log^2 n)
   * Space - O(log n)
   */
  def diameter: List[A] = {
    def build(t: Tree[A], p: List[A]): List[A] = 
      if (t.isEmpty) p
      else if (t.left.height > t.right.height) build(t.left, t.value :: p)
      else build(t.right, t.value :: p)

    if (isEmpty) Nil
    else {
      val ld = left.diameter
      val rd = right.diameter
      val md = if (ld.length > rd.length) ld else rd
      if (1 + left.height + right.height > md.length)
        build(right, value :: build(left, Nil).reverse).reverse
      else md
    }
  }

  /**
   * Searches for the n-th element of this list.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def apply(n: Int): A = 
    if (isEmpty) throw new NoSuchElementException("Tree doesn't contain a " + n + "th element.")
    else {
      val size = left.size
      if (n < size) left(n)
      else if (n > size) right(n - size - 1)
      else value
    }

  /**
   * Converts this tree into the string representation.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  override def toString: String = 
    if (isEmpty) "."
    else "{" + left + value + right + "}"

  /**
   * Converts this tree into linked list.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def toList: List[A] = {
    def loop(t: Tree[A], l: List[A]): List[A] = 
      if (t.isEmpty) l
      else loop(t.left, t.value :: loop(t.right, l))

    loop(this, Nil)
  }
}

object Leaf extends Tree[Nothing] {
  def value: Nothing = throw new NoSuchElementException("Leaf.value")
  def left: Tree[Nothing] = throw new NoSuchElementException("Leaf.left")
  def right: Tree[Nothing] = throw new NoSuchElementException("Leaf.right")
  def size: Int = 0

  def isEmpty: Boolean = true
}

class Branch[A <% Ordered[A]](v: A, l: Tree[A], r: Tree[A], s: Int) extends Tree[A] {
  def value: A = v
  def left: Tree[A] = l
  def right: Tree[A] = r
  def size: Int = s

  def isEmpty: Boolean = false
}

object Tree {

  /**
   * Returns an empty tree instance.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def empty[A]: Tree[A] = Leaf

  /**
   * Creates a singleton tree for given element 'x'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def apply[A <% Ordered[A]](x: A, l: Tree[A] = Leaf, r: Tree[A] = Leaf): Tree[A] = 
    new Branch(x, l, r, l.size + r.size + 1)

  /**
   * Creates a new tree from given sequence 'xs'.
   *
   * Time - O(n log n)
   * Space - O(log n)
   */
  def apply[A <% Ordered[A]](xs: A*): Tree[A] = {
    var r: Tree[A] = Tree.empty
    for (x <- xs) r = r.add(x)
    r
  }

  /**
   * Creates a new tree from given sorted array 'a'.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fromSortedArray[A <% Ordered[A]](a: Array[A]): Tree[A] = {
    def loop(l: Int, r: Int): Tree[A] =
      if (l == r) Tree.empty
      else {
        val p = (l + r) / 2
        Tree(a(p), loop(l, p), loop(p + 1, r))
      }

    loop(0, a.length)
  }
}

var t = Tree.fromSortedArray(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
println(t)
println(t.diameter)