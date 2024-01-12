package task1.hierarchy

import task1.{Branch, Leaf, Tree}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}

trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object TypeClasses {
  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
      traverseTree(tree)((value: A) => Leaf(func(value)))
  }

  implicit object TreeApply extends Apply[Tree] {
    override def ap[A, B](funcTree: Tree[A => B])(tree: Tree[A]): Tree[B] = {
      traverseTree(funcTree)((fn: A => B) => map(tree)(fn))
    }
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = TreeFunctor.map(fa)(f)
  }

  implicit object TreeApplicative extends Applicative[Tree] {
    override def pure[A](a: A): Tree[A] = Leaf(a)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = TreeApply.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = TreeFunctor.map(fa)(f)
  }

  implicit object TreeFlatMap extends FlatMap[Tree] {

    override def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
      traverseTree(tree)(v => func(v))

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = TreeApply.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = TreeFunctor.map(fa)(f)
  }

  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](a: A): Tree[A] = TreeApplicative.pure(a)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = TreeFlatMap.flatMap(fa)(f)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = TreeApply.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = TreeFunctor.map(fa)(f)
  }

  private def traverseTree[T, E](tree: Tree[T])(leafFunc: T => Tree[E]): Tree[E] =
    tree match {
      case Leaf(value) => leafFunc(value)
      case Branch(left, right) =>
        Branch(traverseTree(left)(leafFunc), traverseTree(right)(leafFunc))
    }
}
