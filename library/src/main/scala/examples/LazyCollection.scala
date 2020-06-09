package coroutines.examples

import coroutines._
import coroutines.Macros._

//idea taken from https://contributors.scala-lang.org/t/generators-via-continuations-suspendable-functions/3933

class CoroutineLazyCollection[A](co: Coroutine[A]) extends LazyCollection[A] {
    var _next: Option[A] = co.continue()

    def next(): A = {
        if (_next.isEmpty) {
            throw new IllegalAccessError("You should check if this hasNext first. It was not the cause now.")
        }
        val ret = _next.get
        _next = co.continue()
        ret 
    }

    def hasNext: Boolean = _next.isDefined
}

abstract class LazyCollection[A]{ self =>
    def next(): A
    def hasNext: Boolean 

    def filter(p: A => Boolean): LazyCollection[A] = {
        val co = coroutine[A] {
            while (self.hasNext) {
                val a = self.next()
                if (p(a)) yieldval(a)
            }
        }
        new CoroutineLazyCollection[A](co)
    }

    def map[B](f: A => B): LazyCollection[B] = {
        val co = coroutine[B] {
            while (self.hasNext) {
                val a = self.next()
                yieldval(f(a))
            }
        }
        new CoroutineLazyCollection[B](co)
    }

}

class LazyFromIterator[A](it: Iterator[A]) extends LazyCollection[A] {
    def hasNext: Boolean = it.hasNext
    def next(): A = it.next()
}