abstract class Animal {
  def name: String
}

abstract class Pet extends Animal {}

class Cat extends Pet {
  override def name: String = "Cat"
}

class Dog extends Pet {
  override def name: String = "Dog"
}

// `P <: Pet` means that type variable `P` refers to a subtype of `Pet`.
// aka `P` has all the members and functionality of `Pet`, and maybe some extras
class PetContainer[P <: Pet](p: P) {
  def pet: P = p
}

val dogContainer = new PetContainer[Dog](new Dog)
val catContainer = new PetContainer[Cat](new Cat)

// If we wanted to do something similar with another class that only extends `Animal`, it would not compile
class Lion extends Animal {
  override def name: String = "Lion"
}

// type arguments [ammonite.$file.ch6.upper_type_bounds.Lion] do not conform to class PetContainer's type parameter bounds
// val lionContainer = new PetContainer[Lion](new Lion)
