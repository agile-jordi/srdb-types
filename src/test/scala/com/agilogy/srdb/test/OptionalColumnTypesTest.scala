package com.agilogy.srdb.test

import java.sql._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class OptionalColumnTypesTest extends FlatSpec with MockFactory {

  val ps = mock[PreparedStatement]
  val rs = mock[ResultSet]
  val db = new DummyDb(ps, rs)

  behavior of "optional DbType implicit conversion"

  import com.agilogy.srdb.types._

  it should "set null parameters using None values of type Option[T]" in {
    inSequence {
      (ps.setNull(_:Int , _:Int)).expects(1, Types.INTEGER)
    }
    val optInt:Option[Int] = None
    db.prepare(optInt)
  }


  it should "set optinoal parameters using Some(v) of type Option[T]" in {
    inSequence {
      (ps.setInt(_:Int , _:Int)).expects(1,3)
    }
    val optInt:Option[Int] = Some(3)
    db.prepare(optInt)
  }
  
  it should "read null parameters as None of type Option[T]" in {
    inSequence {
      (ps.setInt(_, _)).expects(1, 3)
      (rs.getInt(_: Int)).expects(1).returning(0)
      (rs.wasNull _).expects().returning(true)
    }
    db.prepare(3)
    assert(db.read(rsReader[Option[Int]]) === None)
  }


  it should "read optional parameters as Some(v) of type Option[T]" in {
    inSequence {
      (ps.setInt(_, _)).expects(1, 3)
      (rs.getInt(_: Int)).expects(1).returning(3)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(3)
    assert(db.read(rsReader[Option[Int]]) === Some(3))
  }
}
