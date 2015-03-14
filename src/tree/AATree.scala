/**
 * This file is part of Scalacaster project,
 * https://github.com/vkostyukov/scalacaster and written by Amanj Sherwany,
 * http://www.amanj.me
 *
 * AA tree http://en.wikipedia.org/wiki/AA_tree
 *
 * Performance: (From Wikipedia)
 * The performance of an AA tree is equivalent to the performance of a
 * red-black tree. While an AA tree makes more rotations than a red-black tree,
 * the simpler algorithms tend to be faster, and all of this balances out to
 * result in similar performance. A red-black tree is more consistent in its
 * performance than an AA tree, but an AA tree tends to be flatter, which
 * results in slightly faster search times
 *
 * Insert - O(log n)
 * Lookup - O(log n)  
 * Remove - O(log n)
 */

object AATree {

  sealed abstract class AATree[+T: Ordering] {
    val value: T
    val level: Int
    val left: AATree[T]
    val right: AATree[T]

    def member[B >: T: Ordering](a: B): Boolean = this match {
      case Empty                                                => false
      case Fork(v, _, l, r) if a == v                           => true
      case Fork(v, _, l, r) if implicitly[Ordering[B]].lt(a, v) => l.member(v)
      case Fork(v, _, l, r)                                     => r.member(v)
    }

    def +[B >: T: Ordering](a: B): AATree[B] = this match {
      case Empty                                                 =>
        Fork(a, 1, Empty, Empty)
      case Fork(v, lv, l, r) if implicitly[Ordering[B]].lt(a, v) => 
        Fork(v, lv, l.insert(a), r).rebalanceTree
      case Fork(v, lv, l, r) if implicitly[Ordering[B]].gt(a, v) => 
        Fork(v, lv, l, r.insert(a)).rebalanceTree
      case _                                                     =>
        this
    }

    def insert[B >: T: Ordering](a: B): AATree[B] = insert(a)
    def add[B >: T: Ordering](a: B): AATree[B] = insert(a)

    def insertAll[B >: T: Ordering](as: List[B]): AATree[B] = {
      as.foldLeft(this: AATree[B])((z: AATree[B], y: B) => z + y)
    }

    def merge[B >: T: Ordering](that: AATree[B]): AATree[B] = 
      if(this == Empty) that
      else if(that == Empty) this
      else {
        insertAll(that.toList)
      }

    def ++[B >: T: Ordering](tree: AATree[B]): AATree[B] = merge(tree)

    // Let's make our tree an instance of functor, and monad
    def map[B: Ordering](f: T => B): AATree[B] = this match {
      case Empty            => Empty
      case Fork(v, _, l, r) =>
        val mappedL = l.map(f)
        val mappedR = r.map(f)
        mappedL.merge(mappedR) + f(v)
    }

    def flatMap[B: Ordering](f: T => AATree[B]): AATree[B] = this match {
      case Empty            => Empty
      case Fork(v, _, l, r) =>
        val mappedL = l.flatMap(f)
        val mappedR = r.flatMap(f)
        mappedL.merge(mappedR).merge(f(v))
    }



    def foreach(f: T => Unit): Unit = toList.foreach(f)

    def filter(p: T => Boolean): AATree[T] = Empty.insertAll(toList.filter(p))
     

    def fold[B: Ordering](f: (T, B, B) => B, z: B): B = this match {
      case Empty            => z
      case Fork(v, _, l, r) =>
        f(v, l.fold(f, z), r.fold(f, z))
    }

    def delete[B >: T: Ordering](a: B): AATree[B] = this match {
      case Empty                                                 => 
        Empty
      case Fork(v, lv, l, r) if implicitly[Ordering[B]].gt(a, v) => 
        Fork(v, lv, l, r.delete(a)).rebalanceAfterDelete
      case Fork(v, lv, l, r) if implicitly[Ordering[B]].lt(a, v) => 
        Fork(v, lv, l.delete(a), r).rebalanceAfterDelete
      case Fork(_, _, Empty, Empty)                              =>
        Empty
      case Fork(_, lv, Empty, r)                                 =>
        val succ = successor
        val r2 = r.delete(succ)
        Fork(succ, lv, Empty, r2).rebalanceAfterDelete
      case Fork(_, lv, l, r)                                     =>
        val pred = predecessor
        val l2 = l.delete(pred)
        Fork(pred, lv, l2, r).rebalanceAfterDelete
    }

    def -[B >: T: Ordering](a: B): AATree[B] = delete(a) 

    def toList[B >: T: Ordering]: List[B] = {
      value::(left.toList ++ right.toList)      
    }


    // some helper functions
    protected def rebalanceTree[B >: T: Ordering]: AATree[B] = skew.split

    protected def rebalanceAfterDelete[B >: T: Ordering]: AATree[B] = {
      val t1 = decreaseLevel.skew
      val t2 = t1.right.skew
      val t3 = t2 match {
        case Fork(v1, lv1, l1, Fork(v2, lv2, l2, r2)) =>
          Fork(v1, lv1, l1, Fork(v2, lv2, l2, r2.skew))
        case _                                        =>
          t2
      }
      val t4 = t3.split
      t4 match {
        case Fork(v, lv, l, r) => Fork(v, lv, l, r.split)
        case _                 => t4
      }
    }

    protected def successor[B >: T: Ordering]: B = {
      def successor2(t: AATree[B]): B = this match {
        case Fork(v, _, Empty, _)       => v
        case Fork(_, _, l, _)           => successor2(l)
        case _                                   =>
          // Should never happen, but to silent the warning
          throw new Exception("Empty node does not have a successor")
      }
      this match {
        case Fork(_, _, _, Fork(v, _, Empty, _)) => v
        case Fork(_, _, _, Fork(v, _, l, _))     => successor2(l)
        case _                                   =>
          throw new Exception("Empty node does not have a successor")
      }
    }

    protected def predecessor[B >: T: Ordering]: B = {
      def predecessor2(t: AATree[B]): B = this match {
        case Fork(v, _, _, Empty)            => v
        case Fork(v, _, _, r)                => predecessor2(r) 
        case _                                   =>
          throw new Exception("Empty node does not have a predecessor")
      }
      this match {
        case Fork(v, _, Fork(_, _, _, Empty), _) => v
        case Fork(_, _, Fork(_, _, _, r), _)     => predecessor2(r)
        case _                                   =>
          // Should never happen, but to silent the warning
          throw new Exception("Empty node does not have a predecessor")
      }
    }
    protected def skew[B >: T: Ordering]: AATree[B] = this match {
      case Empty                              =>
        Empty
      case Fork(v, _, Empty, r)               => 
        this
      case Fork(v, lv, l, r) if lv == l.level =>
        Fork(l.value, l.level, l.left, Fork(v, lv, l.right, r))
      case _                                  => 
        this
    }
    protected def split[B >: T: Ordering]: AATree[B] = this match {
      case Empty                                     => this
      case Fork(v, lv, l, Empty)                     => this
      case Fork(v, lv, l, Fork(_, _, _, Empty))      => this
      case Fork(v, lv, l, r) if lv == r.right.level  =>
        Fork(r.value, r.level+1, Fork(v, lv, l, r.left), r.right)
      case _                                         => this
    }
    protected def decreaseLevel[B >: T: Ordering]: AATree[B] = {
      val newLevel = 1 + Math.min(left.level, right.level)
      this match {
        case Fork(v, lv, l, r) if newLevel < lv =>
          val r2 = r match {
            case Fork(vr, lvr, lr, rr) if newLevel < lvr =>
              Fork(vr, newLevel, lr, rr)
            case _                                       =>
              r
          }
          Fork(v, newLevel, l, r2)
      }
    }
  }

  private final case class Fork[+T: Ordering](value: T, level: Int,
    left: AATree[T], right: AATree[T]) extends AATree[T]

  private final case object Empty extends AATree[Nothing] {
    val value: Nothing         = 
      throw new Exception("Empty Tree has no value")
    val level: Int             = 0
    val left: AATree[Nothing]  = Empty
    val right: AATree[Nothing] = Empty
  }


  implicit def nothingOrdering[T <: Nothing]: Ordering[Nothing] = 
    Ordering.by((e: T) => true)


  def empty[B: Ordering]: AATree[B] = Empty
  def apply[B: Ordering](a: B*): AATree[B] = Empty.insertAll(a.toList)
}
