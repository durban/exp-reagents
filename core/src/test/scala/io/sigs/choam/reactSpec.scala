package io.sigs.choam

import java.util.concurrent.{ LinkedBlockingDeque, ConcurrentLinkedQueue }
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.JavaConverters._

import cats.implicits._
import cats.effect.IO

import fs2.async
import kcas._

class ReactSpecNaiveKCAS
  extends ReactSpec
  with SpecNaiveKCAS

class ReactSpecCASN
  extends ReactSpec
  with SpecCASN

class ReactSpecMCAS
  extends ReactSpec
  with SpecMCAS

abstract class ReactSpec extends BaseSpec {

  import React._

  "Simple CAS" should "work as expected" in {
    val ref = Ref.mk("ert")
    val rea = lift((_: Int).toString) × (ref.cas("ert", "xyz") >>> lift(_ => "boo"))
    val (s1, s2) = rea.unsafePerform((5, ()))
    s1 should === ("5")
    s2 should === ("boo")
    ref.invisibleRead.unsafeRun should === ("xyz")
  }

  "Combined updates" should "indeed be atomic" in {
    val r1 = Ref.mk[List[Int]](Nil)
    val r2 = Ref.mk[List[Int]](Nil)

    def pushBoth(i: Int): Unit = {
      def push(r: Ref[List[Int]], i: Int): React[Unit, Unit] = {
        r.upd[Unit, Unit] { (l, _) =>
          (i :: l, ())
        }
      }
      val r = push(r1, i) * push(r2, i)
      r.unsafeRun
      ()
    }

    def pushAll(maxSize: Long): Unit = {
      for (_ <- 1L to maxSize) {
        val i = java.util.concurrent.ThreadLocalRandom.current().nextInt()
        pushBoth(i)
      }
    }

    val n = 80000L
    val m = 50000L
    val tsk = for {
      f1a <- async.start(IO { pushAll(n) })
      f1b <- async.start(IO { pushAll(n) })
      f2 <- async.start(IO { pushAll(m) })
      _ <- f1a
      _ <- f1b
      _ <- f2
    } yield ()
    tsk.unsafeRunSync()

    val l1 = r1.invisibleRead.unsafeRun
    val l2 = r2.invisibleRead.unsafeRun
    l1.length.toLong should === (2 * n + m)
    l2.length.toLong should === (2 * n + m)
    for ((i1, i2) <- l1 zip l2) {
      i1 should === (i2)
    }
  }

  "updWith" should "perform the chained action atomically" in {
    val N = 100000
    val r1 = Ref.mk("foo")
    val r2 = Ref.mk("bar")
    val sw = React.swap(r1, r2)

    sw.unsafeRun
    r1.invisibleRead.unsafeRun should === ("bar")
    r2.invisibleRead.unsafeRun should === ("foo")
    sw.unsafeRun
    r1.invisibleRead.unsafeRun should === ("foo")
    r2.invisibleRead.unsafeRun should === ("bar")

    val tsk = for {
      f1 <- async.start(IO { for (_ <- 1 to N) sw.unsafeRun })
      f2 <- async.start(IO { for (_ <- 1 to N) sw.unsafeRun })
      _ <- f1
      _ <- f2
    } yield ()
    tsk.unsafeRunSync()
    r1.invisibleRead.unsafeRun should === ("foo")
    r2.invisibleRead.unsafeRun should === ("bar")
  }

  it should "behave correctly when used through modifyWith" in {
    val r1 = Ref.mk("foo")
    val r2 = Ref.mk("x")
    val r = r1.modifyWith { ov =>
      if (ov eq "foo") React.ret("bar")
      else r2.upd[Unit, String] { (o2, _) =>
        (ov, o2)
      }
    }

    r.unsafeRun
    r1.invisibleRead.unsafeRun should === ("bar")
    r2.invisibleRead.unsafeRun should === ("x")

    r.unsafeRun
    r1.invisibleRead.unsafeRun should === ("x")
    r2.invisibleRead.unsafeRun should === ("bar")
  }

  "consistentRead" should "indeed be consistent" in {
    val N = 100000
    val r1 = Ref.mk("foo")
    val r2 = Ref.mk("bar")
    val cr = React.consistentRead(r1, r2)
    val sw = React.swap(r1, r2)

    val tsk = for {
      f1 <- async.start(IO { for (_ <- 1 to N) sw.unsafeRun })
      f2 <- async.start(IO {
        for (_ <- 1 to N) {
          val (v1, v2) = cr.unsafeRun
          assert(((v1 eq "foo") && (v2 eq "bar")) || ((v1 eq "bar") && (v2 eq "foo")))
        }
      })
      _ <- f1
      _ <- f2
    } yield ()
    tsk.unsafeRunSync()
  }

  def pushAll(r: React[Int, _], count: Int): Unit = {
    for (_ <- 1 to count) {
      val i = java.util.concurrent.ThreadLocalRandom.current().nextInt()
      r.unsafePerform(i)
    }
  }

  def popAll(r: React[Unit, List[Int]], expLen: Int, count: Int, errors: LinkedBlockingDeque[String]): Unit = {
    require(expLen > 0)
    for (_ <- 1 to count) {
      val lst = r.unsafeRun
      if (lst.length =!= expLen) {
        if (lst.length =!= 0) {
          errors.offer(s"actual length ${lst.length} doesn't equal expected length ${expLen}")
        } // else: OK, drained the stacks
      } else if (!lst.forall(_ == lst.head)) {
        errors.offer(s"not all popped items are equal")
      }
    }
  }

  "2 combined stack" should "work atomically" in {
    // TODO: add kill switch to shut down tasks if an assertion fails
    val s1 = new TreiberStack[Int]
    val s2 = new TreiberStack[Int]
    val push = s1.push * s2.push
    val pop = s1.tryPop * s2.tryPop
    val errors = new java.util.concurrent.LinkedBlockingDeque[String](100)
    val n = 8000000
    val m = 7000000
    val tsk = for {
      push1 <- async.start(IO { pushAll(push.rmap(_ => ()), n) })
      push2 <- async.start(IO { pushAll(push, n) })
      pop1 <- async.start(IO { popAll(pop.rmap { case (o1, o2) => o1.toList ++ o2.toList }, expLen = 2, count = m, errors = errors) })
      pop2 <- async.start(IO { popAll(pop.rmap { case (o1, o2) => o1.toList ++ o2.toList }, expLen = 2, count = m, errors = errors) })
      _ <- push1
      _ <- push2
      _ <- pop1
      _ <- pop2
    } yield ()
    tsk.unsafeRunSync()

    if (!errors.isEmpty) {
      fail(s"Errors:\n${errors.asScala.mkString("\n")}")
    }

    val l1 = s1.unsafeToList
    val l2 = s2.unsafeToList
    if (l1 != l2) {
      fail("Different stacks at the end")
    }
  }

  // TODO: figure out how could we actually deadlock
  "Deadlocks" should "not be possible (!!!)" ignore {
    val nStacks = 10
    val stacks = List.fill(nStacks)(new TreiberStack[Int])
    val push = stacks.map(_.push).reduceLeft[React[Int, Unit]]{ (a, b) => (a * b).rmap(_ => ()) }
    val pushFlipped = stacks.reverse.map(_.push).reduceLeft[React[Int, Unit]] { (a, b) => (a * b).rmap(_ => ()) }
    val pop = stacks.map(_.tryPop.rmap(_.toList)).reduceLeft[React[Unit, List[Int]]] { (a, b) => (a * b).rmap { case (x, y) => x ++ y } }
    val popFlipped = stacks.reverse.map(_.tryPop.rmap(_.toList)).reduceLeft[React[Unit, List[Int]]] { (a, b) => (a * b).rmap { case (x, y) => x ++ y } }
    val errors = new java.util.concurrent.LinkedBlockingDeque[String](100)
    val n = 8000000
    val m = 7500000
    val tsk = for {
      push1 <- async.start(IO { pushAll(push, n) })
      push2 <- async.start(IO { pushAll(pushFlipped, n) })
      pop1 <- async.start(IO { popAll(pop, expLen = nStacks, count = m, errors) })
      pop2 <- async.start(IO { popAll(popFlipped, expLen = nStacks, count = m, errors) })
      _ <- push1
      _ <- push2
      _ <- pop1
      _ <- pop2
    } yield ()
    tsk.unsafeRunSync()

    if (!errors.isEmpty) {
      fail(s"Errors:\n${errors.asScala.mkString("\n")}")
    }

    val lsts = stacks.map(_.unsafeToList)
    if (!lsts.forall(lst => lst == lsts.head)) {
      fail("Different stacks at the end")
    }
  }

  "Choice" should "prefer the first option" in {
    val r1 = Ref.mk("r1")
    val r2 = Ref.mk("r2")
    val rea = r1.cas("r1", "x") + r2.cas("r2", "x")
    val res = rea.unsafeRun
    res should === (())
    r1.invisibleRead.unsafeRun should === ("x")
    r2.invisibleRead.unsafeRun should === ("r2")
  }

  it should "use the second option, if the first is not available" in {
    val r1 = Ref.mk("z")
    val r2 = Ref.mk("r2")
    val rea = r1.cas("r1", "x") + (r2.cas("r2", "x") * r1.cas("z", "r1"))
    // r2: "r2" -> "x" AND r1: "z" -> "r1"
    rea.unsafeRun
    r2.invisibleRead.unsafeRun should === ("x")
    r1.invisibleRead.unsafeRun should === ("r1")
    // r1: "r1" -> "x"
    rea.unsafeRun
    r1.invisibleRead.unsafeRun should === ("x")
    r2.invisibleRead.unsafeRun should === ("x")
  }

  it should "work if it's after some other operation" in {
    val r1a = Ref.mk("1a")
    val r1b = Ref.mk("1b")
    val r2a = Ref.mk("2a")
    val r2b = Ref.mk("2b")
    val r3a = Ref.mk("3a")
    val r3b = Ref.mk("3b")
    val rea =
      r1a.cas("1a", "xa") >>>
      r1b.cas("1b", "xb") >>>
      (
       (r2a.cas("2a", "ya") >>> r2b.cas("2b", "yb")) +
       (r3a.cas("3a", "za") >>> r3b.cas("3b", "zb"))
      )

    // 1st choice selected:
    rea.unsafeRun
    r1a.invisibleRead.unsafeRun should === ("xa")
    r1b.invisibleRead.unsafeRun should === ("xb")
    r2a.invisibleRead.unsafeRun should === ("ya")
    r2b.invisibleRead.unsafeRun should === ("yb")
    r3a.invisibleRead.unsafeRun should === ("3a")
    r3b.invisibleRead.unsafeRun should === ("3b")

    r1a.cas("xa", "1a").unsafeRun
    r1b.cas("xb", "1b").unsafeRun
    r1a.invisibleRead.unsafeRun should === ("1a")
    r1b.invisibleRead.unsafeRun should === ("1b")

    // 2nd choice selected:
    rea.unsafeRun
    r1a.invisibleRead.unsafeRun should === ("xa")
    r1b.invisibleRead.unsafeRun should === ("xb")
    r2a.invisibleRead.unsafeRun should === ("ya")
    r2b.invisibleRead.unsafeRun should === ("yb")
    r3a.invisibleRead.unsafeRun should === ("za")
    r3b.invisibleRead.unsafeRun should === ("zb")
  }

  it should "work even if it's computed" in {
    val r1a = Ref.mk("1a")
    val r1b = Ref.mk("1b")
    val r2a = Ref.mk("2a")
    val r2b = Ref.mk("2b")
    val r3a = Ref.mk("3a")
    val r3b = Ref.mk("3b")
    val rea =
      r1a.invisibleRead >>>
      React.computed { s =>
        if (s eq "1a") {
          r1b.cas("1b", "xb") >>> (r2a.cas("2a", "ya") + r3a.cas("3a", "za"))
        } else {
          r1b.cas("1b", "xx") >>> (r2b.cas("2b", "yb") + r3b.cas("3b", "zb"))
        }
      }

    // THEN selected, 1st choice selected:
    rea.unsafeRun
    r1a.invisibleRead.unsafeRun should === ("1a")
    r1b.invisibleRead.unsafeRun should === ("xb")
    r2a.invisibleRead.unsafeRun should === ("ya")
    r2b.invisibleRead.unsafeRun should === ("2b")
    r3a.invisibleRead.unsafeRun should === ("3a")
    r3b.invisibleRead.unsafeRun should === ("3b")

    r1b.cas("xb", "1b").unsafeRun

    // THEN selected, 2nd choice selected:
    rea.unsafeRun
    r1a.invisibleRead.unsafeRun should === ("1a")
    r1b.invisibleRead.unsafeRun should === ("xb")
    r2a.invisibleRead.unsafeRun should === ("ya")
    r2b.invisibleRead.unsafeRun should === ("2b")
    r3a.invisibleRead.unsafeRun should === ("za")
    r3b.invisibleRead.unsafeRun should === ("3b")

    r1a.cas("1a", "xa").unsafeRun
    r1b.cas("xb", "1b").unsafeRun

    // ELSE selected, 1st choice selected:
    rea.unsafeRun
    r1a.invisibleRead.unsafeRun should === ("xa")
    r1b.invisibleRead.unsafeRun should === ("xx")
    r2a.invisibleRead.unsafeRun should === ("ya")
    r2b.invisibleRead.unsafeRun should === ("yb")
    r3a.invisibleRead.unsafeRun should === ("za")
    r3b.invisibleRead.unsafeRun should === ("3b")

    r1b.cas("xx", "1b").unsafeRun

    // ELSE selected, 2nd choice selected:
    rea.unsafeRun
    r1a.invisibleRead.unsafeRun should === ("xa")
    r1b.invisibleRead.unsafeRun should === ("xx")
    r2a.invisibleRead.unsafeRun should === ("ya")
    r2b.invisibleRead.unsafeRun should === ("yb")
    r3a.invisibleRead.unsafeRun should === ("za")
    r3b.invisibleRead.unsafeRun should === ("zb")
  }

  it should "be stack-safe (even when deeply nested)" in {
    val n = 16 * React.maxStackDepth
    val ref = Ref.mk("foo")
    val successfulCas = ref.cas("foo", "bar")
    val fails = (1 to n).foldLeft[React[Unit, Unit]](React.retry) { (r, _) =>
      r + React.retry
    }

    val r: React[Unit, Unit] = fails + successfulCas
    r.unsafeRun should === (())
    ref.getter.unsafeRun should === ("bar")
  }

  it should "be stack-safe (even when deeply nested and doing actual CAS-es)" in {
    val n = 16 * React.maxStackDepth
    val ref = Ref.mk("foo")
    val successfulCas = ref.cas("foo", "bar")
    val refs = Array.fill(n)(Ref.mk("x"))
    val fails = refs.foldLeft[React[Unit, Unit]](React.retry) { (r, ref) =>
      r + ref.cas("y", "this will never happen")
    }

    val r: React[Unit, Unit] = fails + successfulCas
    r.unsafeRun should === (())
    ref.getter.unsafeRun should === ("bar")
    refs.foreach { ref =>
      ref.getter.unsafeRun should === ("x")
    }
  }

  it should "correctly backtrack (1) (no jumps)" in {
    backtrackTest1(2)
  }

  it should "correctly backtrack (1) (even with jumps)" in {
    backtrackTest1(React.maxStackDepth + 1)
  }

  it should "correctly backtrack (2) (no jumps)" in {
    backtrackTest2(2)
  }

  it should "correctly backtrack (2) (even with jumps)" in {
    backtrackTest2(React.maxStackDepth / 4)
  }

  /**                +
   *                / \
   *               /   \
   *              /     \
   *             /       \
   *        [CASx_ok1]  CAS_ok4
   *            |
   *            |
   *            +
   *           / \
   *          /   \
   *         /     \
   *        /				\
   *    CASx_ok2  [CAS_ok3]
   *       |
   *       |
   *       +
   *      / \
   *     /   \
   *    /     \
   *   /       \
   * CAS_fail  Retry
   */
  def backtrackTest1(x: Int) = {
    val (okRefs1, ok1) = mkOkCASes(x, "foo1", "bar1")
    val (okRefs2, ok2) = mkOkCASes(x, "foo2", "bar2")
    val okRef3 = Ref.mk("foo3")
    val okRef4 = Ref.mk("foo4")
    val failRef = Ref.mk("fail")
    val left = ok1 >>> ((ok2 >>> (failRef.cas("x_fail", "y_fail") + React.retry)) + okRef3.cas("foo3", "bar3"))
    val right = okRef4.cas("foo4", "bar4")
    val r = left + right
    r.unsafeRun should === (())
    okRefs1.foreach { ref =>
      ref.getter.unsafeRun should === ("bar1")
    }
    okRefs2.foreach { ref =>
      ref.getter.unsafeRun should === ("foo2")
    }
    okRef3.getter.unsafeRun should === ("bar3")
    okRef4.getter.unsafeRun should === ("foo4")
    failRef.getter.unsafeRun should === ("fail")
  }

  /**            +
   *            / \
   *           /   \
   *          /     \
   *         /       \
   *     CASx_ok   CASx_ok
   *        |         |
   *        |         |
   *        +         +
   *       / \       / \
   *             .
   *             .
   *             .
   *     |              |
   * CAS_leaf0  ... CAS_leaf15
   */
  def backtrackTest2(x: Int): Unit = {

    def oneChoice(leftCont: React[Unit, Unit], rightCont: React[Unit, Unit], x: Int, label: String): (React[Unit, Unit], () => Unit) = {
      val ol = s"old-${label}-left"
      val nl = s"new-${label}-left"
      val (lRefs, left) = mkOkCASes(x, ol, nl)
      val or = s"old-${label}-right"
      val nr = s"new-${label}-right"
      val (rRefs, right) = mkOkCASes(x, or, nr)
      def reset(): Unit = {
        lRefs.foreach { ref => ref.modify(_ => ol).unsafeRun }
        rRefs.foreach { ref => ref.modify(_ => or).unsafeRun }
      }
      (((left >>> leftCont) + (right >>> rightCont)).discard, reset _)
    }

    val leafs = Array.tabulate(16)(idx => Ref.mk(s"foo-${idx}"))

    val (l1, rss1) = leafs.grouped(2).map {
      case Array(refLeft, refRight) =>
        val ol = refLeft.invisibleRead.unsafeRun
        val or = refRight.invisibleRead.unsafeRun
        oneChoice(refLeft.cas(ol, s"${ol}-new"), refRight.cas(or, s"${or}-new"), x, "l1")
      case _ =>
        fail
    }.toList.unzip
    assert(l1.size == 8)

    val (l2, rss2) = l1.grouped(2).map {
      case List(rl, rr) =>
        oneChoice(rl, rr, x, "l2")
      case _ =>
        fail
    }.toList.unzip
    assert(l2.size == 4)

    val (l3, rss3) = l2.grouped(2).map {
      case List(rl, rr) =>
        oneChoice(rl, rr, x, "l3")
      case _ =>
        fail
    }.toList.unzip
    assert(l3.size == 2)

    val (top, rs) = oneChoice(l3(0), l3(1), x, "top")

    def reset(): Unit = {
      rss1.foreach(_())
      rss2.foreach(_())
      rss3.foreach(_())
      rs()
    }

    def checkLeafs(expLastNew: Int): Unit = {
      for ((ref, idx) <- leafs.zipWithIndex) {
        val expContents = if (idx <= expLastNew) s"foo-${idx}-new" else s"foo-${idx}"
        val contents = ref.invisibleRead.unsafeRun
        contents should === (expContents)
      }
    }

    checkLeafs(-1)
    for (e <- 0 until leafs.size) {
      top.unsafeRun
      checkLeafs(e)
      reset()
    }
  }

  def mkOkCASes(n: Int, ov: String, nv: String): (Array[Ref[String]], React[Unit, Unit]) = {
    val ref0 = Ref.mk(ov)
    val refs = Array.fill(n - 1)(Ref.mk(ov))
    val r = refs.foldLeft(ref0.cas(ov, nv)) { (r, ref) =>
      (r * ref.cas(ov, nv)).discard
    }
    (ref0 +: refs, r)
  }

  "Post-commit actions" should "be executed" in {
    val r1 = Ref.mk("x")
    val r2 = Ref.mk("")
    val r3 = Ref.mk("")
    val r = r1.upd[Unit, String] {
      case (s, _) =>
        val r = s + "x"
        (r, r)
    }
    val pc1 = r.postCommit(r2.upd[String, Unit] { case (_, x) => (x, ()) })
    val pc2 = pc1.postCommit(r3.upd[String, Unit] { case (_, x) => (x, ()) })

    pc1.unsafeRun should === ("xx")
    r1.invisibleRead.unsafeRun should === ("xx")
    r2.invisibleRead.unsafeRun should === ("xx")
    r3.invisibleRead.unsafeRun should === ("")

    pc2.unsafeRun should === ("xxx")
    r1.invisibleRead.unsafeRun should === ("xxx")
    r2.invisibleRead.unsafeRun should === ("xxx")
    r3.invisibleRead.unsafeRun should === ("xxx")
  }

  // TODO: this is a conflicting CAS
  "Popping then pushing back" should "work (???)" ignore {
    val stack = new TreiberStack[Int]
    val popPush = stack.tryPop.rmap(_.getOrElse(0)) >>> stack.push

    stack.unsafeToList should === (List())
    stack.push.unsafePerform(1)
    stack.unsafeToList should === (List(1))

    // FIXME:
    popPush.unsafeRun
    stack.unsafeToList should === (List(1, 1))
  }

  // TODO: this is a conflicting CAS
  "Impossible CAS" should "work (???)" ignore {
    val ref = Ref.mk("foo")
    val cas1 = ref.cas("foo", "bar")
    val cas2 = ref.cas("foo", "baz")
    val r = cas1 >>> cas2
    r.unsafeRun

    // FIXME:
    ref.invisibleRead.unsafeRun should === ("baz")
  }

  "Michael-Scott queue" should "work correctly" in {
    val q = new MichaelScottQueue[String]
    q.unsafeToList should === (Nil)

    q.tryDeque.unsafeRun should === (None)
    q.unsafeToList should === (Nil)

    q.enqueue.unsafePerform("a")
    q.unsafeToList should === (List("a"))

    q.tryDeque.unsafeRun should === (Some("a"))
    q.unsafeToList should === (Nil)
    q.tryDeque.unsafeRun should === (None)
    q.unsafeToList should === (Nil)

    q.enqueue.unsafePerform("a")
    q.unsafeToList should === (List("a"))
    q.enqueue.unsafePerform("b")
    q.unsafeToList should === (List("a", "b"))
    q.enqueue.unsafePerform("c")
    q.unsafeToList should === (List("a", "b", "c"))

    q.tryDeque.unsafeRun should === (Some("a"))
    q.unsafeToList should === (List("b", "c"))

    q.enqueue.unsafePerform("x")
    q.unsafeToList should === (List("b", "c", "x"))

    q.tryDeque.unsafeRun should === (Some("b"))
    q.unsafeToList should === (List("c", "x"))
    q.tryDeque.unsafeRun should === (Some("c"))
    q.unsafeToList should === (List("x"))
    q.tryDeque.unsafeRun should === (Some("x"))
    q.tryDeque.unsafeRun should === (None)
    q.unsafeToList should === (Nil)
  }

  it should "allow multiple producers and consumers" in {
    val max = 10000
    val q = new MichaelScottQueue[String]
    val produce = IO {
      for (i <- 0 until max) {
        q.enqueue.unsafePerform(i.toString)
      }
    }
    val cs = new ConcurrentLinkedQueue[String]
    val stop = new AtomicBoolean(false)
    val consume = IO {
      def go(): Unit = {
        q.tryDeque.unsafeRun match {
          case Some(s) =>
            cs.offer(s)
            go()
          case None =>
            if (stop.get()) () // we're done
            else go()
        }
      }
      go()
    }
    val tsk = for {
      p1 <- async.start(produce)
      c1 <- async.start(consume)
      p2 <- async.start(produce)
      c2 <- async.start(consume)
      _ <- p1
      _ <- p2
      _ <- IO { stop.set(true) }
      _ <- c1
      _ <- c2
    } yield ()

    try {
      tsk.unsafeRunSync()
    } finally {
      stop.set(true)
    }

    cs.asScala.toVector.sorted should === (
      (0 until max).toVector.flatMap(n => Vector(n.toString, n.toString)).sorted
    )
  }

  "Integration with IO" should "work" in {
    val act: IO[String] = for {
      ref <- React.newRef[String]("foo").run[IO]
      _ <- ref.upd { (s, p: String) => (s + p, ()) }[IO]("bar")
      res <- ref.getter.run[IO]
    } yield res

    act.unsafeRunSync() should === ("foobar")
    act.unsafeRunSync() should === ("foobar")
  }
}
