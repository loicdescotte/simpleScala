import collection.mutable.Stack
import org.scalatest._
import models._

class PersonTest extends FlatSpec with Matchers {

  import org.scalactic._

  "Validation" should "validate persons" in {
    import Person._
    parsePerson("Loic", "34") should be(Good(Person("Loic", 34)))
    parsePerson("Loic", "-1") should be(Bad(One(""""-1" is not a valid age""")))
    parsePerson("", "-1") should be(Bad(Many(""""" is not a valid name""", """"-1" is not a valid age""")))
    parsePerson("", "34 years old") should be(Bad(Many(""""" is not a valid name""", """"34 years old" is not a valid integer""")))
  }

}