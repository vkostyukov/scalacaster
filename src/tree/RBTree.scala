/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Red-Black Tree http://en.wikipedia.org/wiki/Red-black_tree
 *
 * Insert - O(log n)
 * Lookup - O(log n)  
 * Remove - O(log n)
 *
 * -Notes-
 *
 * This is an implementation of Okasaki's red-black tree introduced in his fantastic book
 * "Purely Functional Data Structures and Algorithms". Here is the short link to article:
 * 
 *            www.ccs.neu.edu/course/cs3500wc/jfp99redblack.pdf
 *
 * As well, as Okasaki's tree this implementation doesn't contain a 'remove()' method, 
 * since it totally madness to understand and write such code. There is one CS hero, 
 * who handled that task - Matt Might. He described a new approach of removing node 
 * from red-black tree by using additional node colors (double black, negate black)
 * and new phase - 'bubbling'. Anyway, these things are a bit more then 'just-for-fun' 
 * package. Here is the link to Matt's research:
 *
 *              http://matt.might.net/articles/red-black-delete/
 *
 * Chris Okasaki said in his blog that removing is awkward operation for functional
 * data structures due to persistence. What does 'remove' mean in a functional world?
 * It means that the client wants to get a new collection without one element. In functional 
 * settings we can store both states of collection - without element 'x' (before insertion) 
 * and with 'x' (after insertion). So, when the removing is needed for 'x' the previous
 * state of collection can be used instead.
 *
 * Anyway, red-black trees is a great data structure with fantastic running time 
 * algorithms, but it's implementation is so complicated that you might use simple
 * BST instead. This is why, Robert Sedgewick invented left-leaning red-black trees as
 * simple (in terms of implementation) replacement for red-black trees. Here is his 
 * awesome trees description:
 * 
 *               http://www.cs.princeton.edu/~rs/talks/LLRB/LLRB.pdf
 *
 * There are also two ways to improve current implementation. First of them - modify
 * 'balance()' method to expect violations only along the search path. Second - use 
 * 'two-way-comparison' that is invented by Anre Andersonin method 'contains()'.
 * Here is the explanation of this idea:
 *
 *                  http://user.it.uu.se/~arnea/ps/searchproc.pdf
 *
 * This tree implementation doesn't inherit loads a useful methods from binary search
 * tree (see 'src/tree/Tree.scala') since is not necessary to have these methods in 
 * both trees (it also brakes DRY principle). The main goal of this project - is to
 * provide a clear functional approaches of implementation functional data structures,
 * not to provide a completely stable collections and algorithms framework.
 *
 *
 * PS: I would be happy, if someone decided to add Matt's 'remove()' method into this
 *     implementation.
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
   * Checks whether this tree contains element 'x' or not.
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
   * Time - O(log n)
   * Space - O(log n)
   */
  private def balancedAdd[B >: A <% Ordered[B]](x: B): RBTree[B] = {
    def balance[B >: A <% Ordered[B]](t: RBTree[B], l: RBTree[B], r: RBTree[B]): RBTree[B] =
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

    if (isEmpty) RedTree(x)
    else if (x < value) balance(this, left.balancedAdd(x), right)
    else if (x > value) balance(this, left, right.balancedAdd(x))
    else this
  }

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
   * Converts this tree into the string representation.
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
