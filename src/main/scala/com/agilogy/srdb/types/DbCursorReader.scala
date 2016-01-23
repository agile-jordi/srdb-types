package com.agilogy.srdb.types

import java.sql.ResultSet
import com.agilogy.srdb.types
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

class Cursor(val rs: ResultSet) {

  var _done = !rs.next()

  def done: Boolean = _done

  def next(): Unit = {
    require(!done)
    _done = !rs.next()
  }
}

trait DbCursorReader[RT] {

  private[types] def read(rs: ResultSet): RT

  def readAndNext(c: Cursor): RT = {
    val res = read(c.rs)
    c.next()
    res
  }

  def map[T2](f: (RT) => T2): DbCursorReader[T2] = MappedDbCursorReader(this, f)
}

trait DbCursorReaderOps {

  self =>

  implicit class DbCursorResultSetOps(rs: ResultSet) {
    def toSeq[T: DbCursorReader]: Seq[T] = self.toSeq[T](rs)
    def toOption[T: DbCursorReader]: Option[T] = self.toOption[T](rs)
    def foreach[T: DbCursorReader](cb: T => Unit): Unit = self.foreach[T](rs)(cb)
  }

  implicit def cursorReader[T: DbReader]: DbCursorReader[T] = new DbCursorReader[T] {
    override private[types] def read(rs: ResultSet): T = implicitly[DbReader[T]].get(rs)
  }

  def toSeq[T: DbCursorReader](rs: ResultSet): Seq[T] = {
    val res = new ListBuffer[T]
    foreach[T](rs)(v => res.append(v))
    res.toSeq
  }

  def foreach[T: DbCursorReader](rs: ResultSet)(cb: T => Unit): Unit = {
    val cr = implicitly[DbCursorReader[T]]
    val c = new Cursor(rs)
    while (!c.done) {
      cb(cr.readAndNext(c))
    }
  }

  def toOption[T: DbCursorReader](rs: ResultSet): Option[T] = {
    val cr = implicitly[DbCursorReader[T]]
    val c = new Cursor(rs)
    if (c.done) {
      None
    } else {
      val res = Some(cr.readAndNext(c))
      if (!c.done) throw new IllegalArgumentException("The query returned more than one row")
      res
    }
  }

}

case class SimpleDbCursorReader[RT](rowReader: DbReader[RT]) extends DbCursorReader[RT] {

  def join[RT2](implicit r2: DbCursorReader[RT2]): GroupLeftJoinReads[RT, RT2] = GroupLeftJoinReads(this, r2)

  def leftJoin[RT2](implicit r2: DbCursorReader[RT2]): GroupLeftJoinReads[RT, RT2] = GroupLeftJoinReads(this, r2)

  def joinOne[RT2](implicit r2: DbCursorReader[RT2]): JoinReads[RT, RT2] = JoinReads(this, r2)

  def leftJoinOne[RT2](implicit r2: DbCursorReader[RT2]): LeftJoinReads[RT, RT2] = LeftJoinReads(this, r2)

  private[types] def read(rs: ResultSet): RT = rowReader.get(rs)

  override def map[T2](f: (RT) => T2): DbCursorReader[T2] = new SimpleDbCursorReader(rowReader.map(f))
}

object SimpleDbCursorReader {
  implicit def simpleCursorReader[RT](dbReader: DbReader[RT]): SimpleDbCursorReader[RT] = SimpleDbCursorReader[RT](dbReader)
}

case class JoinReads[T1, T2](left: SimpleDbCursorReader[T1], right: DbCursorReader[T2]) extends DbCursorReader[(T1, T2)] {

  private[types] override def read(rs: ResultSet): (T1, T2) = {
    val leftResult = left.read(rs)
    val rightResult = right.read(rs)
    leftResult -> rightResult
  }
}

case class LeftJoinReads[T1, T2](left: SimpleDbCursorReader[T1], right: DbCursorReader[T2]) extends DbCursorReader[(T1, Option[T2])] {

  private[types] override def read(rs: ResultSet): (T1, Option[T2]) = {
    val leftResult = left.read(rs)
    val rightResult = try {
      Some(right.read(rs))
    } catch {
      //TODO: Unify exception handling
      case ncr: types.NullColumnReadException => None
    }
    leftResult -> rightResult

  }

}

case class GroupLeftJoinReads[T1, T2](groupReads: SimpleDbCursorReader[T1], right: DbCursorReader[T2]) extends DbCursorReader[(T1, Seq[T2])] {

  protected[this] def readGroup(rs: ResultSet) = groupReads.read(rs)

  private[types] override def read(rs: ResultSet): (T1, Seq[T2]) = groupReads.read(rs) -> Seq.empty

  override def readAndNext(c: Cursor): (T1, Seq[T2]) = {
    @tailrec
    def rec(group: T1, elements: Seq[T2]): (T1, Seq[T2]) = {
      val currentGroup = readGroup(c.rs)
      if (currentGroup == group) {
        val r = try {
          Some(right.readAndNext(c))
        } catch {
          case ncre: types.NullColumnReadException =>
            c.next()
            None
        }
        val currentElements = elements ++ r
        if (c.done) {
          group -> currentElements
        } else {
          rec(currentGroup, currentElements)
        }
      } else {
        group -> elements
      }
    }

    require(!c.done)
    rec(readGroup(c.rs), Seq.empty)
  }

  //  override def readUsingAliases: GroupLeftJoinReads[T1, T2] = GroupLeftJoinReads(groupReads.readUsingAliases, right.readUsingAliases)
}

case class MappedDbCursorReader[RT, RT2](reads: DbCursorReader[RT], f: RT => RT2) extends DbCursorReader[RT2] {

  override def read(rs: ResultSet): RT2 = f(reads.read(rs))

  override def map[T2](f: (RT2) => T2): DbCursorReader[T2] = MappedDbCursorReader(reads, this.f.andThen(f))
}