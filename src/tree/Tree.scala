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
 */

abstract class Tree[+A <% Ordered[A]] {
  def value: A
  def left: Tree[A]
  def right: Tree[A]

  def isEmpty: Boolean

  def isValid: Boolean =
    if (isEmpty) true
    else if (left.isEmpty && right.isEmpty) true
    else if (left.isEmpty) right.value >= value && right.isValid
    else if (right.isEmpty) left.value <= value && left.isValid
    else left.value <= value && right.value >= value && left.isValid && right.isValid

  def isBalanced: Boolean = 
    math.abs(left.height - right.height) <= 1

  def add[B >: A <% Ordered[B]](x: B): Tree[B]  =
    if (isEmpty) new Node(x, Leaf, Leaf)
    else if(x <= value) new Node(value, left.add(x), right)
    else new Node(value, left, right.add(x))

  def remove[B >: A <% Ordered[B]](x: B): Tree[B] =
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) new Node(value, left.remove(x), right)
    else if (x > value) new Node(value, left, right.remove(x))
    else {
      if (left.isEmpty && right.isEmpty) Leaf
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else {
        val succ = successor(x)
        new Node(succ, left, right.remove(succ))
      }
    }

  def contains[B >: A <% Ordered[B]](x: B): Boolean =
    if (isEmpty) false
    else if (x < value) left.contains(x)
    else if (x > value) right.contains(x)
    else true

  def subtree[B >: A <% Ordered[B]](x: B): Tree[B] =
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) left.subtree(x)
    else if (x > value) right.subtree(x)
    else this

  def isSubtree[B >: A <% Ordered[B]](t: Tree[B]): Boolean = ???

  def foreach(f: (A) => Unit): Unit = 
    if (!isEmpty) {
      left.foreach(f)
      f(value)
      right.foreach(f)
    }

  def fold[B](n: B)(op: (B, A) => B): B = {
    def loop(t: Tree[A], a: B): B =
      if (t.isEmpty) a
      else loop(t.right, op(loop(t.left, a), t.value))

    loop(this, n)
  }

  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)
  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

  def size: Int =
    if (isEmpty) 0
    else 1 + left.size + right.size

  def min: A = 
    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (left.isEmpty) value
    else left.min

  def max: A = 
    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (right.isEmpty) value
    else right.max

  def height: Int =
    if (isEmpty) 0
    else 1 + math.max(left.height, right.height)

  def successor[B >: A <% Ordered[B]](x: B): A = {
    def forward(t: Tree[A], p: List[Tree[A]]): A =
      if (t.isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
      else if (x < t.value) forward(t.left, t :: p)
      else if (x > t.value) forward(t.right, t :: p)
      else if (!t.right.isEmpty) t.right.min
      else backward(t, p)

    def backward(t: Tree[A], p: List[Tree[A]]): A = 
      if (p.isEmpty) throw new NoSuchElementException("The " + x + " doesn't have an accessor.")
      else if (t == p.head.right) backward(p.head, p.tail)
      else p.head.value

    forward(this, Nil)
  }

  def predecessor[B >: A <% Ordered[B]](x: B): A = {
    def forward(t: Tree[A], p: List[Tree[A]]): A =
      if (t.isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
      else if (x < t.value) forward(t.left, t :: p)
      else if (x > t.value) forward(t.right, t :: p)
      else if (!t.left.isEmpty) t.left.max
      else backward(t, p)

    def backward(t: Tree[A], p: List[Tree[A]]): A = 
      if (p.isEmpty) throw new NoSuchElementException("The " + x + " doesn't have an accessor.")
      else if (t == p.head.left) backward(p.head, p.tail)
      else p.head.value

    forward(this, Nil)
  }

  def nthMax(n: Int): A = apply(size - n - 1)
  def nthMin(n: Int): A = apply(n)

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

  def apply(n: Int): A = 
    if (isEmpty) throw new NoSuchElementException("Tree doesn't contain a " + n + "th element.")
    else {
      val size = left.size
      if (n < size) left.nthMin(n)
      else if (n > size) right.nthMin(n - size - 1)
      else this.value
    }

  override def toString: String = 
    if (isEmpty) "."
    else "{" + left + value + right + "}"

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

  def isEmpty: Boolean = true
}

class Node[A <% Ordered[A]](v: A, l: Tree[A], r: Tree[A]) extends Tree[A] {
  def value: A = v
  def left: Tree[A] = l
  def right: Tree[A] = r

  def isEmpty: Boolean = false
}

object Tree {
  def apply[A <% Ordered[A]](xs: A*): Tree[A] = {
    var r: Tree[A] = Leaf
    for (x <- xs) r = r.add(x)
    r
  }

  def fromSortedArray[A <% Ordered[A]](a: Array[A]): Tree[A] = {
    def loop(t: Tree[A], l: Int, r: Int): Tree[A] =
      if (l == r) t
        else {
        val p = (l + r) / 2
        loop(loop(t.add(a(p)), l, p), p + 1, r)
      }

    loop(Leaf, 0, a.length)
  }
}

var t = Tree.fromSortedArray(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
println(t.predecessor(1))
