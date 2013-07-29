/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Linked List http://en.wikipedia.org/wiki/Linked_list
 *
 * Prepend - O(1)
 * Append - O(n)
 * Head - O(1)
 * Tail - O(1)
 * Lookup - O(n)
 */

abstract class List[+A] {

  /**
   * The head of this list.
   */
  def head: A

  /**
   * The tail of this list.
   */
  def tail: List[A]

  /**
   * Checks whether this list is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Appends the element 'x' to this list.
   * 
   * Time - O(n)
   * Space - O(n)
   */ 
  def append[B >: A](x: B): List[B] =
    if (isEmpty) List(x)
    else List(head, tail.append(x))

  /**
   * Prepends the element 'x' to this list. 
   *
   * Time - O(1)
   * Space - O(1)
   */
  def prepend[B >: A](x: B): List[B] = List(x, this)

  /**
   * Concatenates this list with given 'xs' list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def concat[B >: A](xs: List[B]): List[B] = 
    if (isEmpty) xs
    else tail.concat(xs).prepend(head)

  /**
   * Removes the element 'x' from the list.
   * 
   * Time - O(n)
   * Space - O(n)
   */
  def remove[B >: A](x: B): List[B] = 
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this list.")
    else if (x != head) List(head, tail.remove(x))
    else tail

  /**
   * Searches for the n-th element of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def apply(n: Int): A =
    if (isEmpty) throw new NoSuchElementException("Index out of the bounds.")
    else if (n == 0) head
    else tail(n - 1)

  /**
   * Checks whether this list contains element 'x' or not.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def contains[B >: A](x: B): Boolean = 
    if (isEmpty) false
    else if (x != head) tail.contains(x)
    else true

  /**
   * Applies the 'f' function to the each element of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def foreach(f: (A) => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  /**
   * Combines all elements of this list into value.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def fold[B](n: B)(op: (B, A) => B): B = {
    def loop(l: List[A], a: B): B =
      if (l.isEmpty) a
      else loop(l.tail, op(a, l.head))

    loop(this, n)
  }

  /**
   * Creates new list by mapping this list to the 'f' function.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def map[B](f: (A) => B): List[B] = 
    if (isEmpty) Nill
    else tail.map(f).prepend(f(head))

  /**
   * Caclulates the sum of all elements of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  /**
   * Calculates the product of all elements of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

  /**
   * Searches for the minimal element of this list.
   * 
   * Time - O(n)
   * Space - O(n)
   */ 
  def min[B >: A](implicit ordering: Ordering[B]): B = 
    if (isEmpty) throw new NoSuchElementException("Nill.min")
    else if (tail.isEmpty) head
    else ordering.min(head, tail.min(ordering))

  /**
   * Searches for the maximal element of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def max[B >: A](implicit ordering: Ordering[B]): B = 
    if (isEmpty) throw new NoSuchElementException("Nill.max")
    else if (tail.isEmpty) head
    else ordering.max(head, tail.max(ordering))

  /**
   * Slices this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def slice(from: Int, until: Int): List[A] = 
    if (isEmpty || until == 0) Nill
    else if (from == 0) tail.slice(from, until - 1).prepend(head)
    else tail.slice(from - 1, until - 1)

  /**
   * Reverses this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def reverse: List[A] = { 
    def loop(s: List[A], d: List[A]): List[A] = 
      if (s.isEmpty) d
      else loop(s.tail, d.prepend(s.head))

    loop(this, Nill)
  }

  /**
   * Shuffles this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def shuffle: List[A] = {
    val random = new scala.util.Random
    def insert(x: A, ll: List[A], n: Int): List[A] = 
      ll.slice(0, n).concat(ll.slice(n, ll.length).prepend(x))

    if (isEmpty) Nill
    else insert(head, tail.shuffle, random.nextInt(tail.length + 1))
  }

  /**
   * Generates variations of this list with given length 'k'.
   * 
   * NOTES: To count number of variations the following formula can be used:
   * 
   * V_k,n = n!/(n - k)!
   *
   * Time - O(V_k,n)
   * Space - O(V_k,n)
   */
  def variations(k: Int): List[List[A]] = {
    def mixmany(x: A, ll: List[List[A]]): List[List[A]] =
      if (ll.isEmpty) Nill
      else foldone(x, ll.head).concat(mixmany(x, ll.tail))

    def foldone(x: A, ll: List[A]): List[List[A]] = 
      (1 to ll.length).foldLeft(List(ll.prepend(x)))((a, i) => a.prepend(mixone(i, x, ll)))

    def mixone(i: Int, x: A, ll: List[A]): List[A] = 
      ll.slice(0, i).concat(ll.slice(i, ll.length).prepend(x))

    if (isEmpty || k > length) Nill
    else if (k == 1) map(List(_))
    else mixmany(head, tail.variations(k - 1)).concat(tail.variations(k))
  }

  /**
   * Generates all permutations of this list.
   *
   * NOTES: To count number of permutations the following formula can be used:
   *
   * P_n = V_n,n = n!
   *
   * Time - O(P_n)
   * Space - O(P_n)
   */
  def permutations: List[List[A]] = 
    (2 to length).foldLeft(variations(1))((a, i) => variations(i).concat(a))

  /**
   * Searches for the longest increasing sub list of this list.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-3-longest-increasing-subsequence/
   *
   * Time - O(n^2)
   * Space - O(n)
   */
  def longestIncreasingSubsequence[B >: A](implicit ordering: Ordering[B]): List[B] = {
    // We can use the followng instead:
    // zipWithIndex.map(t => (t._2, List(t._1))).toMap
    // http://stackoverflow.com/questions/17828431/convert-scalas-list-into-map-with-indicies-as-keys
    def init(i: Int, l: List[A], m: Map[Int, List[A]]): Map[Int, List[A]] = 
     if (l.isEmpty) m
     else init(i + 1, l.tail, m + (i -> List(l.head)))

    def loop(i: Int, l: List[A], m: Map[Int, List[A]]): List[A] =
      if (l.isEmpty) m.maxBy(_._2.length)._2.reverse
      else {
        val f = m.filter(p => p._1 < i && ordering.lt(p._2.head, l.head))
        if (f.isEmpty) loop(i + 1, l.tail, m)
        else {
          val (_, ll) = f.maxBy(_._2.length)
          loop(i + 1, l.tail, m + (i -> ll.prepend(l.head)))
        }
      }

    if (isEmpty) Nill
    else loop(1, tail, init(0, this, Map[Int, List[A]]()))
  }

  /**
   * Searches for the longest common subsequence of this and 'l' lists.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-4-longest-common-subsequence/
   *
   * TODO: The DP approach can be used here to reduce the compexity to O(mn)
   *
   * Time - O(2^n)
   * Space - O(n)
   */
  def longestCommonSubsequence[B >: A](l: List[B]): List[B] = {
    def loop(a: List[A], b: List[B], c: List[B]): List[B] = 
      if (a.isEmpty || b.isEmpty) c
      else if (a.head == b.head) loop(a.tail, b.tail, c.prepend(a.head))
      else {
        val la = loop(a.tail, b, c)
        val lb = loop(a, b.tail, c)
        if (la.length > lb.length) la else lb
      }

    loop(reverse, l.reverse, Nill)
  }

  /**
   * Returns the number of inversions that required to make this list sorted.
   *
   * http://www.geeksforgeeks.org/counting-inversions/
   *
   * TODO: The Divide-And-Conquer approach can be used here to reduce the complexity to O(n log n)
   *       due to merge sort.
   *
   * Time - O(n^2)
   * Space - O(n)
   */
  def inversions[B >: A](implicit ordering: Ordering[B]): Int = {
    def loop(x: A, t: List[A], i: Int): Int =
      if (t.isEmpty) i
      else if (ordering.gt(x, t.head)) loop(x, t.tail, i + 1)
      else loop(x, t.tail, i)

    if (isEmpty) 0 
    else tail.inversions(ordering) + loop(head, tail, 0)
  }

  /**
   * Count the largest sum of contigous sub list.
   *
   * http://www.geeksforgeeks.org/largest-sum-contiguous-subarray/
   * 
   * NOTES: It uses the DP-approach based on Kadane’s algorithm.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def largestSumOfContiguousSubList[B >: A](implicit num: Numeric[B]): B = {
    def loop(sm: B, gm: B, l: List[B]): B = 
      if (l.isEmpty) gm
      else {
        val nsm = num.max(l.head, num.plus(sm, l.head))
        loop(nsm, num.max(gm, nsm), l.tail)
      }

    if (isEmpty) throw new NoSuchElementException("Empty list.")
    else loop(head, head, tail)
  }

  /**
   * Generates all the subsequences of this list.
   *
   * Time - O(2^n)
   * Space - O(n)
   */
  def subsequences: List[List[A]] =
    if (isEmpty) Nill
    else { 
      val ss = tail.subsequences 
      ss.map(_.prepend(head)).prepend(List(head)).concat(ss)
    }

  /**
   * Builds the inreasing subsequence with maximum sum.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-12-longest-palindromic-subsequence/
   *
   * Time - O()
   * Space - O()
   */
  def maximumSumIncreasingSubsequence: List[A] = ???

  /**
   * Calculates the length of this list.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def length: Int = 
    if (isEmpty) 0 
    else 1 + tail.length

  /**
   * Converts this list into the string representation.
   * 
   * Time - O(n)
   * Space - O(n)
   */
  override def toString: String = {
    def loop(h: A, t: List[A], s: String): String = 
      if (!t.isEmpty) loop(t.head, t.tail, s + h + ", ")
      else s + h

    if (isEmpty) "List[]"
    else "List[" + loop(head, tail, "") + "]"
  }
}

object Nill extends List[Nothing] { // since 'Nil' already reserved 
  def head: Nothing = throw new NoSuchElementException("Nill.head")
  def tail: List[Nothing] = throw new NoSuchElementException("Nill.tail") 

  def isEmpty: Boolean = true
}

class Cons[A](h: A, t: List[A] = Nill) extends List[A] {
  def head: A = h
  def tail: List[A] = t

  def isEmpty: Boolean = false
}

object List {

  /**
   * Returns an empty list instance.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def empty[A]: List[A] = Nill

  /**
   * Creates a new list from given head 'h' and tail 't'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def apply[A](h: A, t: List[A] = Nill): List[A] = new Cons(h, t)

  /**
   * Creates a new list from given 'xs' sequence.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def apply[A](xs: A*): List[A] = {
    var r: List[A] = Nill
    for (x <- xs.reverse) r = r.prepend(x)
    r
  }
}
