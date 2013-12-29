/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * -Notes-
 * 
 * Usage example:
 * var g: Graph[Int, String] = Graph(
 *        ("A", 20, "B"), ("B", 30, "C"), 
 *        ("C", 40, "D"), ("D", 50, "A"), 
 *        ("A", 100, "C"), ("D", 200, "B"))
 * 
 * Creates the following graph:
 * 
 *  |<-------------------|
 *  D ---> A ---> B ---> C 
 *  |------|----->|      |
 *         |------------>|
 *
 */

case class Edge[E, N](source: Graph[E, N], target: Graph[E, N], value: E)

class Graph[E, N](var value: N = null.asInstanceOf[N]) {

  import scala.collection.immutable.Queue

  var inEdges: List[Edge[E, N]] = Nil
  var outEdges: List[Edge[E, N]] = Nil

  /**
   * All successors of this graph.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def succs: List[Graph[E, N]] = outEdges.map(_.target)

  /**
   * All predecessors of this graph.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def preds: List[Graph[E, N]] = inEdges.map(_.source)

  /**
   * Adds new connection to this graph.
   *
   * Time - O()
   * Space - O()
   */
  def connect(from: N, via: E, to: N): (Graph[E, N], Graph[E, N]) = {
    val fromGraph: Graph[E, N] = if (value == null) {value = from; this} else hop(from) match {
      case Some(g) => g
      case None => Graph.one(from)
    }

    val toGraph: Graph[E, N] = hop(to) match {
      case Some(g) => g
      case None => Graph.one(to)
    }

    fromGraph.outEdges = new Edge(fromGraph, toGraph, via) :: fromGraph.outEdges
    toGraph.inEdges = new Edge(fromGraph, toGraph, via) :: toGraph.inEdges

    (fromGraph, toGraph)
  }

  /**
   * Hops to the given node 'n' if its exist in this graph.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def hop(n: N): Option[Graph[E, N]] = graphsByDepth.find(_.value == n)

  /**
   * Returns all nodes of this graph arranged by DFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def nodesByDepth: List[N] = graphsByDepth.map(_.value)

  /**
   * Returns all nodes of this graph arranged by BFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def nodesByBreadth: List[N] = graphsByBreadth.map(_.value)

  /**
   * Returns all graphs that are connected to this graph arranged by DFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def graphsByDepth: List[Graph[E, N]] = {
    def loop(g: Graph[E, N], s: Set[Graph[E, N]]): Set[Graph[E, N]] = 
      if (!s(g)) {
        val ss = g.succs.foldLeft(s + g)((acc, gg) => loop(gg, acc))
        g.preds.foldLeft(ss)((acc, gg) => loop(gg, acc))
      } else s

    loop(this, Set()).toList
  }

  /**
   * Returns all graphs that are connected to this graph arranged by BFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def graphsByBreadth: List[Graph[E, N]] = {
    def loop(q: Queue[Graph[E, N]], s: Set[Graph[E, N]]): Set[Graph[E, N]] = 
      if (!q.isEmpty && !s(q.head)) {
        val qq = q.head.succs.foldLeft(q.tail)((acc, gg) => acc :+ gg)
        loop(q.head.preds.foldLeft(qq)((acc, gg) => acc :+ gg), s + q.head)
      } else s

    loop(Queue(this), Set()).toList
  }

  /**
   * Returns the number of nodes connected with this graph.
   *
   * Time - O()
   * Space - O()
   */
  def size: Int = graphsByDepth.size

  override def equals(o: Any): Boolean = o match {
    case g: Graph[_, _] => g.value == value
    case _ => false
  }

  override def toString: String = "Graph(" + value + ")"
}

class WeightedGraph[N](n: N) extends Graph[Double, N](n) {

  /**
   * Searches for the shortest path in this graph.
   *
   * Time - O()
   * Space - O()
   */
  def dijkstra(from: N, to: N): List[N] = {
    var distances = nodesByDepth.map((_, Double.PositiveInfinity)).toMap + (from -> 0.0)
    var spt: Map[N, N] = Map()

    while (!distances.isEmpty) {
      // search for min in distances
      // remove min from distances

      // for all succsesors of mininum update its distance v--e--u
      // if dist(v) + e < dist(u)
      // dist(u) = dist(v) + e
      // spt + (u -> v)
      // v - is min

      val (v, _) = distances.minBy(_._2)
      distances = distances - v

      val vv = hop(v).get
      for (e <- vv.outEdges) {
        val u = e.target.value
        if (distances(u) < distances(v) + e.value) {
          distances = distances + (u -> (distances(v) + e.value))
          spt = spt + (u -> v)
        }
      }
    }

    var step = to
    var path = List(step)
    while (spt.contains(step)) {
      step = spt(step)
      path = step :: path
    }

    path.reverse
  }
}

object Graph {
  def apply[E, N](tuples: (N, E, N)*): Graph[E, N] = {
    val g: Graph[E, N] = Graph.empty
    for ((from, via, to) <- tuples) {
      g.connect(from, via, to)
    }
    g
  }

  def one[E, N](n: N): Graph[E, N] = new Graph(n)

  def empty[E, N]: Graph[E, N] = new Graph
}

// var g: Graph[Int, String] = Graph(
//        ("A", 20, "B"), ("B", 30, "C"), 
//        ("C", 40, "D"), ("D", 50, "A"), 
//        ("A", 100, "C"), ("D", 200, "B"))

// println(g.nodesByBreadth)
