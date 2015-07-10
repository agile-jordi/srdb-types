package com.agilogy.srdb.types

import java.sql.Types

/**
 * A type according to the JDBC specification
 *
 * This class is only used because JDBC, until 3.0, uses `Int`s  stead of typed instances
 *
 * @param code The JDBC `Int` code of the JDBC type
 * @group API
 */
sealed abstract class JdbcType(val code: Int, val name: String)

/** @group API */
object JdbcType {
  case object TinyInt extends JdbcType(Types.TINYINT, "TINYINT")
  case object SmallInt extends JdbcType(Types.SMALLINT, "SMALLINT")
  case object Integer extends JdbcType(Types.INTEGER, "INTEGER")
  case object BigInt extends JdbcType(Types.BIGINT, "BIGINT")

  case object Boolean extends JdbcType(Types.BOOLEAN, "BOOLEAN")

  case object Varchar extends JdbcType(Types.VARCHAR, "VARCHAR")
  case object Char extends JdbcType(Types.CHAR, "CHAR")
  case object LongVarchar extends JdbcType(Types.LONGNVARCHAR, "LONGNVARCHAR")

  case object Date extends JdbcType(Types.DATE, "DATE")
  case object Time extends JdbcType(Types.TIME, "TIME")
  case object Timestamp extends JdbcType(Types.TIMESTAMP, "TIMESTAMP")
  //  case object TimestampTZ extends JdbcType(Types.TIMESTAMP_WITH_TIMEZONE, "TIMESTAMPTZ")

  case object Numeric extends JdbcType(Types.NUMERIC, "NUMERIC")
  case object Decimal extends JdbcType(Types.DECIMAL, "DECIMAL")
  case object Float extends JdbcType(Types.FLOAT, "FLOAT")
  case object Double extends JdbcType(Types.DOUBLE, "DOUBLE")

  case object Array extends JdbcType(Types.ARRAY, "ARRAY")

  case object Other extends JdbcType(Types.OTHER, "OTHER")

}

