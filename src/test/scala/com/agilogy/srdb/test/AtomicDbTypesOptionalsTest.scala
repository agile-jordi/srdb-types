package com.agilogy.srdb.test

import java.sql._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class AtomicDbTypesOptionalsTest extends FlatSpec with MockFactory {

  val ps = mock[PreparedStatement]
  val rs = mock[ResultSet]

  behavior of "optional DbType implicit conversion"

  import com.agilogy.srdb.types._

  it should "set null parameters using None values of type Option[T]" in {
    inSequence {
      (ps.setNull(_:Int , _:Int)).expects(1, Types.INTEGER)
    }
    val optInt:Option[Int] = None
    set(ps,optInt)
  }


  it should "set optinoal parameters using Some(v) of type Option[T]" in {
    inSequence {
      (ps.setInt(_:Int , _:Int)).expects(1,3)
    }
    val optInt:Option[Int] = Some(3)
    set(ps,optInt)
  }
  
  it should "read null parameters as None of type Option[T]" in {
    inSequence {
      (ps.setInt(_, _)).expects(1, 3)
      (rs.getInt(_: Int)).expects(1).returning(0)
      (rs.wasNull _).expects().returning(true)
      (rs.getInt(_: String)).expects("c").returning(0)
      (rs.wasNull _).expects().returning(true)
    }
    set(ps,3)
    assert(get[Option[Int]](rs) === None)
    assert(get(rs)(optional[Int]("c")) === None)
  }


  it should "read optional parameters as Some(v) of type Option[T]" in {
    inSequence {
      (ps.setInt(_, _)).expects(1, 3)
      (rs.getInt(_: Int)).expects(1).returning(3)
      (rs.wasNull _).expects().returning(false)
      (rs.getInt(_: String)).expects("c").returning(3)
      (rs.wasNull _).expects().returning(false)
    }
    set(ps,3)
    assert(get[Option[Int]](rs) === Some(3))
    assert(get(rs)(optional[Int]("c")) === Some(3))
  }
}
