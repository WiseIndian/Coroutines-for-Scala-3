package coroutines.examples

import coroutines._
import coroutines.Macros._

//idea taken from https://contributors.scala-lang.org/t/generators-via-continuations-suspendable-functions/3933

class CoroutineLazyCollection[A](co: Coroutine[A]) extends LazyCollection[A] {
    var _next: Option[A] = co.continue()

    def next(): A = {
        if (_next.isEmpty) {
            throw new IllegalStateException("You should check if this hasNext first. It was not the case now.")
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

    private inline def generate[A](inline body: Any): LazyCollection[A] = {
        val co = coroutine[A] { body }
        new CoroutineLazyCollection[A](co)
    }

    def filter(p: A => Boolean): LazyCollection[A] = generate[A] {
        while (self.hasNext) {
            val a = self.next()
            if (p(a)) yieldval(a)
        }
    } 
    

    def map[B](f: A => B): LazyCollection[B] = generate[B] {
        while (self.hasNext) {
            val a = self.next()
            yieldval(f(a))
        }
    }

    def flatMap[B](f: A => IterableOnce[B]): LazyCollection[B] = generate[B] {
        while (self.hasNext) {
            val it = f(self.next()).iterator

            while (it.hasNext) yieldval(it.next())
        }
    }

    def dropWhile(p: A => Boolean): LazyCollection[A] = generate[A] {
        var done = false
        while(self.hasNext && !done) {
            val next = self.next()
            if (!p(next)) {
                yieldval(next)
                while(self.hasNext) {
                    yieldval(self.next())
                }
                done = true
            }
        }
    }

}

class LazyFromIterator[A](it: Iterator[A]) extends LazyCollection[A] {
    def hasNext: Boolean = it.hasNext
    def next(): A = it.next()
}